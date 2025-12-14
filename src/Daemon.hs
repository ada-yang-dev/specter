{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&), first)
import Control.Category ((>>>))
import Control.Concurrent (forkIO, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Exception qualified as E
import Control.Lens hiding ((.=), (|>))
import Control.Monad (forever, guard, mfilter, void, when)
import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.Text hiding (try)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char (isDigit)

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Endo (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Network.Socket
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.IO
import System.Posix.Pty
import System.Posix.Signals (sigINT, sigKILL, sigTERM, signalProcess)
import System.Posix.Signals.Exts (sigWINCH)
import System.Process (ProcessHandle, getPid, waitForProcess)
import Prelude hiding (takeWhile)

data Attrs = Attrs
  { _attrsFg :: !Word8,
    _attrsBg :: !Word8,
    _attrsIntensity :: !Word8,
    _attrsUnderline :: !Word8,
    _attrsInverse :: !Bool,
    _attrsItalic :: !Bool,
    _attrsStrike :: !Bool
  }
  deriving (Show, Eq, Ord)

makeLenses ''Attrs

type Cell = (Char, Attrs)

blankAttrs = Attrs 7 0 0 0 False False False

data TermLine = TermLine {_lineCells :: !(V.Vector Cell), _lineHasNewline :: !Bool}
  deriving (Show, Eq, Ord)

makeLenses ''TermLine

type TermLines = StrictSeq TermLine

blankLine width = TermLine (V.replicate width (' ', blankAttrs)) False

blankLineWith width attrs = TermLine (V.replicate width (' ', attrs)) False

newtype StrictSeq a = StrictSeq (Seq a)
  deriving (Show, Eq, Ord, Functor, Semigroup, Monoid, Foldable)

tlEmpty = StrictSeq Seq.empty

tlLength (StrictSeq v) = Seq.length v

tlReplicate n x = x `seq` StrictSeq (Seq.replicate n x)

tlIndex i = lens getter setter
  where
    getter (StrictSeq v) = Seq.index v (max 0 $ min (Seq.length v - 1) i)
    setter (StrictSeq v) val = val `seq` StrictSeq (Seq.update (max 0 $ min (Seq.length v - 1) i) val v)

tlTake i (StrictSeq v) = StrictSeq (Seq.take i v)

tlTakeLast i (StrictSeq v) = StrictSeq (Seq.drop (Seq.length v - i) v)

tlDrop i (StrictSeq v) = StrictSeq (Seq.drop i v)

tlToSeq (StrictSeq v) = v

data CursorState = CursorState {_wrapNext :: !Bool, _origin :: !Bool}
  deriving (Show, Eq, Ord)

data SavedCursor = SavedCursor {_savedRow :: !Int, _savedCol :: !Int, _savedAttrs :: !Attrs, _savedOrigin :: !Bool}
  deriving (Show, Eq, Ord)

data Term = Term
  { _termAttrs :: !Attrs,
    _cursorRow :: !Int,
    _cursorCol :: !Int,
    _cursorState :: !CursorState,
    _savedCursor :: !SavedCursor,
    _cursorVisible :: !Bool,
    _modeWrap :: !Bool,
    _insertMode :: !Bool,
    _altScreenActive :: !Bool,
    _numCols :: !Int,
    _numRows :: !Int,
    _scrollTop :: !Int,
    _scrollBottom :: !Int,
    _scrollBackLines :: !TermLines,
    _viewportOffset :: !Int,
    _termScreen :: !TermLines,
    _termAlt :: !TermLines
  }
  deriving (Show, Eq, Ord)

makeLenses ''CursorState
makeLenses ''SavedCursor
makeLenses ''Term

mkTerm (w, h) = Term blankAttrs 0 0 (CursorState False False) (SavedCursor 0 0 blankAttrs False) True True False False w h 0 (h - 1) tlEmpty 0 screen screen
  where screen = tlReplicate h (blankLine w)

activeScreen :: Lens' Term TermLines
activeScreen = lens getter setter
  where
    getter t = if _altScreenActive t then _termAlt t else _termScreen t
    setter t v = if _altScreenActive t then t {_termAlt = v} else t {_termScreen = v}

cursorLine :: Lens' Term TermLine
cursorLine = lens getter setter
  where
    getter t = t ^. activeScreen . tlIndex (_cursorRow t)
    setter t v = activeScreen . tlIndex (_cursorRow t) .~ v $ t

cursorLineCells :: Lens' Term (V.Vector Cell)
cursorLineCells = cursorLine . lineCells

vIndex i = lens (\v -> v V.! clamp v) (\v x -> v V.// [(clamp v, x)]) where clamp v = max 0 $ min (V.length v - 1) i

addScrollBackLines newLines = scrollBackLines %~ ((<> newLines) >>> tlTakeLast 1000)

resetViewport = viewportOffset .~ 0

scrollViewport delta t = t & viewportOffset %~ clamp . (+ delta)
  where clamp = max 0 . min (tlLength (t ^. scrollBackLines))

processInputEsc input term = case input of
  "\ESC[5;2~" -> ("", scrollViewport (term ^. numRows) term)
  "\ESC[6;2~" -> ("", scrollViewport (-(term ^. numRows)) term)
  "\ESC[1;2A" -> ("", scrollViewport 1 term)
  "\ESC[1;2B" -> ("", scrollViewport (-1) term)
  "\ESC[5;5~" -> ("", scrollViewport (term ^. numRows `div` 2) term)
  "\ESC[6;5~" -> ("", scrollViewport (-(term ^. numRows `div` 2)) term)
  "\ESC[1;2H" -> ("", scrollViewport maxBound term)
  "\ESC[1;2~" -> ("", scrollViewport maxBound term)
  "\ESC[4;2~" -> ("", scrollViewport minBound term)
  "\ESC[1;2F" -> ("", scrollViewport minBound term)
  _ -> (input, term)

renderViewport term = T.concat [renderLine r | r <- [0 .. lastRow]] <> "\ESC[0m"
  where
    (rows, cols) = (term ^. numRows, term ^. numCols)
    offset = term ^. viewportOffset
    showCursor = offset == 0 && term ^. cursorVisible
    (cursorR, cursorC) = (term ^. cursorRow, term ^. cursorCol)
    (screen, scrollback) = (term ^. activeScreen, term ^. scrollBackLines)
    (scrollLen, scrollSeq) = (tlLength scrollback, tlToSeq scrollback)

    getTermLine r =
      let vline = scrollLen - offset + r
       in if
            | vline < 0 -> blankLine cols
            | vline < scrollLen -> scrollSeq `Seq.index` vline
            | otherwise -> screen ^. tlIndex (vline - scrollLen)
    getCell r = safeIndex (getTermLine r ^. lineCells)
    hasNewline r = getTermLine r ^. lineHasNewline
    isFull r = lastCol r == cols - 1
    safeIndex line i = fromMaybe (' ', blankAttrs) $ line V.!? i

    lastCol r = max cursorEnd contentEnd
      where
        cursorEnd = if showCursor && r == cursorR then cursorC else -1
        contentEnd = fromMaybe (-1) $ V.findIndexR (/= (' ', blankAttrs)) (getTermLine r ^. lineCells)

    lastRow
      | term ^. altScreenActive = rows - 1
      | otherwise = max cursorR $ fromMaybe 0 $ listToMaybe [r | r <- [rows - 1, rows - 2 .. 0], lastCol r >= 0]

    withCursor r c (ch, attrs)
      | showCursor, r == cursorR, c == cursorC = (ch, attrs & attrsInverse %~ not)
      | otherwise = (ch, attrs)

    -- Soft-wrap: line full without logical newline
    softWrapMarker = "\ESC[?7w"

    renderLine r
      | term ^. altScreenActive = renderCells blankAttrs [getCell r c | c <- [0 .. cols - 1]] <> "\n"
      | otherwise =
          let cells = [withCursor r c (getCell r c) | c <- [0 .. lastCol r]]
              lineEnd = if hasNewline r || not (isFull r) then "\n" else softWrapMarker
           in renderCells blankAttrs cells <> lineEnd

    renderCells _ [] = ""
    renderCells prev ((ch, attrs) : rest) = attrsSGR prev attrs <> T.singleton ch <> renderCells attrs rest

    attrsSGR prev cur
      | prev == cur = ""
      | otherwise = "\ESC[" <> T.intercalate ";" (filter (not . T.null) codes) <> "m"
      where
        codes = [intCode, italCode, ulCode, invCode, strikeCode, fgCode, bgCode]
        toggle l on off = if cur ^. l /= prev ^. l then if cur ^. l then on else off else ""
        intCode = case (cur ^. attrsIntensity, prev ^. attrsIntensity) of (0, p) | p /= 0 -> "22"; (1, _) -> "1"; (2, _) -> "2"; _ -> ""
        italCode = toggle attrsItalic "3" "23"
        ulCode = case (cur ^. attrsUnderline, prev ^. attrsUnderline) of (0, p) | p /= 0 -> "24"; (1, _) -> "4"; (2, _) -> "21"; _ -> ""
        invCode = toggle attrsInverse "7" "27"
        strikeCode = toggle attrsStrike "9" "29"
        effective a = if a ^. attrsInverse then (a ^. attrsBg, a ^. attrsFg) else (a ^. attrsFg, a ^. attrsBg)
        (fg, bg) = effective cur
        (pfg, pbg) = effective prev
        fgCode = if fg == pfg then "" else "38;5;" <> showT fg
        bgCode = if bg == pbg then "" else "48;5;" <> showT bg

data DECPrivateMode = DECOM | DECAWM | DECTCEM | AltScreen | AltScreenSaveCursor
  deriving (Show, Eq, Ord)

intToDECPrivateMode = (`lookup` [(6, DECOM), (7, DECAWM), (25, DECTCEM), (47, AltScreen), (1047, AltScreen), (1049, AltScreenSaveCursor)])

data TermAtom
  = TermAtomVisibleChar !Char
  | TermAtomSingleCharacterFunction !SingleCharacterFunction
  | TermAtomEscapeSequence !EscapeSequence
  | TermAtomUnknown !Text
  deriving (Show, Eq)

data SingleCharacterFunction = ControlBell | ControlBackspace | ControlCarriageReturn | ControlLineFeed | ControlTab
  deriving (Show, Eq, Ord, Enum, Bounded)

data EscapeSequence
  = EscReverseIndex
  | EscRIS
  | EscDECSC
  | EscDECRC
  | EscDECPAM
  | EscDECPNM
  | EscCSI !CSI
  | EscOSC !OSC
  deriving (Show, Eq)

data CSI
  = CSICursorUp !Int
  | CSICursorDown !Int
  | CSICursorForward !Int
  | CSICursorBack !Int
  | CSICursorPosition !Int !Int
  | CSICursorColumn !Int
  | CSICursorRow !Int
  | CSIEraseInLine !EraseInLineParam
  | CSIEraseInDisplay !EraseInDisplayParam
  | CSIInsertBlankCharacters !Int
  | CSIInsertBlankLines !Int
  | CSIDeleteChars !Int
  | CSIDeleteLines !Int
  | CSIScrollUp !Int
  | CSIScrollDown !Int
  | CSIEraseCharacters !Int
  | CSISoftTerminalReset
  | CSIDECSTBM !(Maybe Int) !(Maybe Int)
  | CSIDECSET !DECPrivateMode
  | CSIDECRST !DECPrivateMode
  | CSISGR ![SGR]
  | CSIDeviceStatusReport !Int
  | CSIDA1
  | CSISetMode !Int
  | CSIResetMode !Int
  deriving (Show, Eq)

data EraseInLineParam = ClearFromCursorToEndOfLine | ClearFromCursorToBeginningOfLine | ClearEntireLine
  deriving (Show, Eq, Ord, Enum, Bounded)

data EraseInDisplayParam = EraseBelow | EraseAbove | EraseAll | EraseSavedLines
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype OSC = OSCSetTitle Text deriving (Show, Eq, Ord)

data SGR
  = SGRReset
  | SGRBold
  | SGRFaint
  | SGRItalic
  | SGRNoItalic
  | SGRUnderline
  | SGRDoubleUnderline
  | SGRInverse
  | SGRNoInverse
  | SGRStrike
  | SGRNoStrike
  | SGRNormal
  | SGRNoUnderline
  | SGRFgColor !Word8
  | SGRBgColor !Word8
  deriving (Show, Eq)

parseTermAtom = parseVisibleChar <|> parseControl

parseVisibleChar = TermAtomVisibleChar <$> satisfy (not . isCtrl)

parseControl = do
  c <- anyChar
  if c == '\ESC'
    then parseEscape
    else pure $ maybe (TermAtomUnknown (T.singleton c)) TermAtomSingleCharacterFunction (singleCharacterFunction c)

singleCharacterFunction = (`lookup` [('\a', ControlBell), ('\b', ControlBackspace), ('\r', ControlCarriageReturn), ('\n', ControlLineFeed), ('\t', ControlTab), ('\f', ControlLineFeed), ('\v', ControlLineFeed)])

parseEscape = anyChar >>= \case
  '[' -> parseCsi
  ']' -> parseOsc
  c -> pure $ maybe (TermAtomUnknown $ "\ESC" <> T.singleton c) TermAtomEscapeSequence $ lookup c [('7', EscDECSC), ('8', EscDECRC), ('M', EscReverseIndex), ('c', EscRIS), ('=', EscDECPAM), ('>', EscDECPNM)]

parseCsi = do
  str <- takeTill (between (0x40, 0x7E) . fromEnum)
  c <- anyChar
  let input = str <> T.singleton c
  pure $ maybe (TermAtomUnknown ("\ESC[" <> input)) (TermAtomEscapeSequence . EscCSI) (processCsi input)

processCsi str = do
  (priv, args, mode) <- parseCsiComponents str
  if priv then parsePrivCsi mode args else parseStdCsi mode args

parseCsiComponents str = case parseOnly (parser <* endOfInput) str of
  Left _ -> Nothing
  Right val -> Just val
  where
    parser = do
      priv <- option False (char '?' >> pure True)
      c <- peekChar'
      args <- if isDigit c || c == ';' then sepBy (option 0 decimal) (char ';') else pure []
      mode <- anyChar
      pure (priv, listToNonEmpty 0 args, mode)

listToNonEmpty def = fromMaybe (def :| []) . NE.nonEmpty

arg1 = max 1 . NE.head

parseStdCsi 'A' args = Just $ CSICursorUp (arg1 args)
parseStdCsi 'B' args = Just $ CSICursorDown (arg1 args)
parseStdCsi 'C' args = Just $ CSICursorForward (arg1 args)
parseStdCsi 'D' args = Just $ CSICursorBack (arg1 args)
parseStdCsi 'H' args = Just $ CSICursorPosition (max 1 (NE.head args)) (max 1 (getArg 1 args))
parseStdCsi 'f' args = Just $ CSICursorPosition (max 1 (NE.head args)) (max 1 (getArg 1 args))
parseStdCsi 'G' args = Just $ CSICursorColumn (arg1 args)
parseStdCsi 'd' args = Just $ CSICursorRow (arg1 args)
parseStdCsi 'K' args =
  CSIEraseInLine <$> case NE.head args of
    0 -> Just ClearFromCursorToEndOfLine
    1 -> Just ClearFromCursorToBeginningOfLine
    2 -> Just ClearEntireLine
    _ -> Nothing
parseStdCsi 'J' args =
  CSIEraseInDisplay <$> case NE.head args of
    0 -> Just EraseBelow
    1 -> Just EraseAbove
    2 -> Just EraseAll
    3 -> Just EraseSavedLines
    _ -> Nothing
parseStdCsi '@' args = Just $ CSIInsertBlankCharacters (arg1 args)
parseStdCsi 'L' args = Just $ CSIInsertBlankLines (arg1 args)
parseStdCsi 'P' args = Just $ CSIDeleteChars (arg1 args)
parseStdCsi 'M' args = Just $ CSIDeleteLines (arg1 args)
parseStdCsi 'S' args = Just $ CSIScrollUp (arg1 args)
parseStdCsi 'T' args = Just $ CSIScrollDown (arg1 args)
parseStdCsi 'X' args = Just $ CSIEraseCharacters (arg1 args)
parseStdCsi 'r' args = Just $ CSIDECSTBM (zeroToNothing (NE.head args)) (zeroToNothing (getArg 1 args))
parseStdCsi 'h' args = Just $ CSISetMode (NE.head args)
parseStdCsi 'l' args = Just $ CSIResetMode (NE.head args)
parseStdCsi 'n' args = Just $ CSIDeviceStatusReport (NE.head args)
parseStdCsi 'c' _ = Just CSIDA1
parseStdCsi 'm' args = Just $ CSISGR (parseSGRCodes (NE.toList args))
parseStdCsi _ _ = Nothing

parsePrivCsi 'h' args = CSIDECSET <$> intToDECPrivateMode (arg1 args)
parsePrivCsi 'l' args = CSIDECRST <$> intToDECPrivateMode (arg1 args)
parsePrivCsi _ _ = Nothing

getArg n (x :| xs) = fromMaybe 0 $ listToMaybe $ drop n (x : xs)

zeroToNothing = mfilter (/= 0) . Just

parseOsc = do
  str <- T.take 66 <$> takeTill (< ' ')
  _ <- option ' ' (char '\a' <|> (string "\ESC\\" >> pure ' '))
  pure $ case T.uncons str >>= \(c, r) -> T.uncons r >>= \(semi, title) -> T.take 64 title <$ guard (c `elem` ("012" :: String) && semi == ';') of
    Just title -> TermAtomEscapeSequence $ EscOSC $ OSCSetTitle title
    Nothing -> TermAtomUnknown $ "\ESC]" <> str

parseSGRCodes = \case
  [] -> [SGRReset]
  38 : 5 : n : rest -> SGRFgColor (fromIntegral $ limit 0 255 n) : parseSGRCodes rest
  48 : 5 : n : rest -> SGRBgColor (fromIntegral $ limit 0 255 n) : parseSGRCodes rest
  38 : 2 : _ : _ : _ : rest -> parseSGRCodes rest
  48 : 2 : _ : _ : _ : rest -> parseSGRCodes rest
  c : rest -> maybe id (:) (sgrCode c) $ parseSGRCodes rest
  where
    sgrCode c = lookup c basic <|> fgColor c <|> bgColor c
    basic = [(0, SGRReset), (1, SGRBold), (2, SGRFaint), (3, SGRItalic), (4, SGRUnderline), (7, SGRInverse), (9, SGRStrike), (21, SGRDoubleUnderline), (22, SGRNormal), (23, SGRNoItalic), (24, SGRNoUnderline), (27, SGRNoInverse), (29, SGRNoStrike), (39, SGRFgColor 7), (49, SGRBgColor 0)]
    fgColor c | c >= 30 && c <= 37 = Just $ SGRFgColor (fromIntegral $ c - 30)
              | c >= 90 && c <= 97 = Just $ SGRFgColor (fromIntegral $ c - 82)
              | otherwise = Nothing
    bgColor c | c >= 40 && c <= 47 = Just $ SGRBgColor (fromIntegral $ c - 40)
              | c >= 100 && c <= 107 = Just $ SGRBgColor (fromIntegral $ c - 92)
              | otherwise = Nothing

isCtrl c = fromEnum c <= 0x1F || c == '\DEL'

processTermAtoms = foldl' (flip processTermAtom)

processTermAtom = \case
  TermAtomVisibleChar c -> processVisibleChar c
  TermAtomSingleCharacterFunction f -> processSCF f
  TermAtomEscapeSequence e -> processEsc e
  TermAtomUnknown _ -> id

processSCF = \case
  ControlBell -> id
  ControlBackspace -> moveCol (subtract 1)
  ControlTab -> \t -> t & cursorCol %~ min (t ^. numCols - 1) . \c -> ((c + 8) `div` 8) * 8
  ControlLineFeed -> processLF
  ControlCarriageReturn -> cursorCol .~ 0

processEsc = \case
  EscReverseIndex -> reverseIndex
  EscRIS -> resetTerm
  EscDECSC -> saveCursor
  EscDECRC -> restoreCursor
  EscCSI csi -> processCSI csi
  _ -> id

saveCursor t = t & savedCursor .~ SavedCursor (t ^. cursorRow) (t ^. cursorCol) (t ^. termAttrs) (t ^. cursorState . origin)

restoreCursor t = t & cursorRow .~ (sc ^. savedRow) & cursorCol .~ (sc ^. savedCol) & termAttrs .~ (sc ^. savedAttrs) & cursorState . origin .~ (sc ^. savedOrigin)
  where sc = t ^. savedCursor

processCSI = \case
  CSICursorUp n -> moveRow (subtract n)
  CSICursorDown n -> moveRow (+ n)
  CSICursorForward n -> moveCol (+ n)
  CSICursorBack n -> moveCol (subtract n)
  CSICursorPosition row col -> cursorMoveAbsoluteTo (row - 1, col - 1)
  CSICursorColumn col -> moveCol $ const (col - 1)
  CSICursorRow row -> setRowAbs (row - 1)
  CSIEraseInLine p -> eraseInLine p
  CSIEraseInDisplay p -> eraseInDisplay p
  CSIInsertBlankCharacters n -> insertBlankChars n
  CSIInsertBlankLines n -> insertBlankLines n
  CSIDeleteChars n -> deleteChars n
  CSIDeleteLines n -> deleteLines n
  CSIScrollUp n -> scrollFromTop termScrollUp n
  CSIScrollDown n -> scrollFromTop termScrollDown n
  CSIEraseCharacters n -> eraseCharacters n
  CSISoftTerminalReset -> resetTerm
  CSIDECSTBM top bot -> setScrollingRegion top bot >>> cursorMoveAbsoluteTo (0, 0)
  CSIDECSET m -> termProcessDec True m
  CSIDECRST m -> termProcessDec False m
  CSISGR sgrs -> termAttrs %~ appEndo (foldMap (Endo . applySGR) sgrs)
  CSISetMode 4 -> insertMode .~ True
  CSIResetMode 4 -> insertMode .~ False
  _ -> id

moveRow f t = cursorMoveTo (f (t ^. cursorRow), t ^. cursorCol) t

moveCol f t = cursorMoveTo (t ^. cursorRow, f (t ^. cursorCol)) t

setRowAbs r t = cursorMoveAbsoluteTo (r, t ^. cursorCol) t

scrollFromTop scroll n t = scroll (t ^. scrollTop) n t

resetTerm t = mkTerm (t ^. numCols, t ^. numRows)

termProcessDec on = \case
  DECOM -> (cursorState . origin .~ on) >>> cursorMoveAbsoluteTo (0, 0)
  DECAWM -> modeWrap .~ on
  DECTCEM -> cursorVisible .~ on
  AltScreen
    | on -> (altScreenActive .~ True) >>> clearAltScreen
    | otherwise -> altScreenActive .~ False
  AltScreenSaveCursor
    | on -> saveCursor >>> (altScreenActive .~ True) >>> clearAltScreen
    | otherwise -> (altScreenActive .~ False) >>> restoreCursor
  where
    clearAltScreen t = t & termAlt .~ tlReplicate (t ^. numRows) (blankLine (t ^. numCols))

applySGR = \case
  SGRReset -> const blankAttrs
  SGRBold -> attrsIntensity .~ 1
  SGRFaint -> attrsIntensity .~ 2
  SGRItalic -> attrsItalic .~ True
  SGRNoItalic -> attrsItalic .~ False
  SGRUnderline -> attrsUnderline .~ 1
  SGRDoubleUnderline -> attrsUnderline .~ 2
  SGRInverse -> attrsInverse .~ True
  SGRNoInverse -> attrsInverse .~ False
  SGRStrike -> attrsStrike .~ True
  SGRNoStrike -> attrsStrike .~ False
  SGRNormal -> attrsIntensity .~ 0
  SGRNoUnderline -> attrsUnderline .~ 0
  SGRFgColor c -> attrsFg .~ c
  SGRBgColor c -> attrsBg .~ c

cursorMoveAbsoluteTo (row, col) t = cursorMoveTo (row + offset, col) t
  where
    offset = if t ^. cursorState . origin then t ^. scrollTop else 0

cursorMoveTo (row, col) t = t & cursorRow .~ limit minY maxY row & cursorCol .~ limit 0 (t ^. numCols - 1) col & cursorState . wrapNext .~ False
  where
    (minY, maxY) = if t ^. cursorState . origin then (t ^. scrollTop, t ^. scrollBottom) else (0, t ^. numRows - 1)

processLF = (cursorLine . lineHasNewline .~ True) >>> addNewline True

reverseIndex t
  | t ^. cursorRow == t ^. scrollTop = termScrollDown (t ^. scrollTop) 1 t
  | otherwise = moveRow (subtract 1) t

eraseInLine p t = clearRegion (r, c1) (r, c2) t
  where
    (r, c) = (t ^. cursorRow, t ^. cursorCol)
    (c1, c2) = case p of
      ClearFromCursorToEndOfLine -> (c, t ^. numCols - 1)
      ClearFromCursorToBeginningOfLine -> (0, c)
      ClearEntireLine -> (0, t ^. numCols - 1)

eraseCharacters n t = clearRegion (r, c) (r, c + n - 1) t where (r, c) = (t ^. cursorRow, t ^. cursorCol)

eraseInDisplay = \case
  EraseAbove -> \t -> clearRegion (0, 0) (t ^. cursorRow, t ^. cursorCol) t
  EraseBelow -> \t -> clearRegion (t ^. cursorRow, t ^. cursorCol) (t ^. numRows - 1, t ^. numCols - 1) t
  EraseAll -> \t -> clearRegion (0, 0) (t ^. numRows - 1, t ^. numCols - 1) t
  EraseSavedLines -> scrollBackLines .~ tlEmpty

insertBlankChars n t = t & cursorLineCells %~ \cells -> V.take col cells <> V.replicate n' (' ', t ^. termAttrs) <> V.slice col (t ^. numCols - col - n') cells
  where
    col = t ^. cursorCol
    n' = limit 0 (t ^. numCols - col) n

insertBlankLines n t
  | between (t ^. scrollTop, t ^. scrollBottom) (t ^. cursorRow) = termScrollDown (t ^. cursorRow) n t
  | otherwise = t

deleteChars n t = t & cursorLineCells %~ \cells -> V.take col cells <> V.slice (col + n') (t ^. numCols - col - n') cells <> V.replicate n' (' ', t ^. termAttrs)
  where
    col = t ^. cursorCol
    n' = limit 0 (t ^. numCols - col) n

deleteLines n t
  | between (t ^. scrollTop, t ^. scrollBottom) (t ^. cursorRow) = termScrollUp (t ^. cursorRow) n t
  | otherwise = t

setScrollingRegion mbTop mbBottom t = ((scrollTop .~ top) >>> (scrollBottom .~ bot)) t
  where
    top' = maybe 0 (subtract 1) mbTop
    bot' = maybe (t ^. numRows - 1) (subtract 1) mbBottom
    top = limit 0 (t ^. numRows - 1) (min top' bot')
    bot = limit 0 (t ^. numRows - 1) (max top' bot')

termScrollDown orig n t = (activeScreen %~ update) t
  where
    n' = limit 0 (t ^. scrollBottom - orig + 1) n
    blank = blankLineWith (t ^. numCols) (t ^. termAttrs)
    update ls = tlTake orig ls <> tlReplicate n' blank <> tlTake (t ^. scrollBottom - orig - n' + 1) (tlDrop orig ls) <> tlDrop (t ^. scrollBottom + 1) ls

termScrollUp orig n t = (copyToScrollBack >>> activeScreen %~ update) t
  where
    n' = limit 0 (t ^. scrollBottom - orig + 1) n
    blank = blankLineWith (t ^. numCols) (t ^. termAttrs)
    copyToScrollBack = if not (t ^. altScreenActive) && orig == 0 then addScrollBackLines (tlTake n' (t ^. termScreen)) else id
    update ls = tlTake orig ls <> tlTake (t ^. scrollBottom - orig - n' + 1) (tlDrop (orig + n') ls) <> tlReplicate n' blank <> tlDrop (t ^. scrollBottom + 1) ls

processVisibleChar c = moveBefore >>> shiftChars >>> setChar >>> moveAfter
  where
    moveBefore t
      | t ^. modeWrap && t ^. cursorState . wrapNext = addNewline True t
      | otherwise = t
    shiftChars t
      | t ^. insertMode && t ^. cursorCol < t ^. numCols - 1 =
          let col = t ^. cursorCol
           in t & cursorLineCells %~ \cells -> V.take (t ^. numCols) (V.take col cells <> V.singleton (' ', blankAttrs) <> V.drop col cells)
      | otherwise = t
    setChar t = t & cursorLineCells . vIndex (t ^. cursorCol) .~ (c, t ^. termAttrs)
    moveAfter t
      | t ^. cursorCol < t ^. numCols - 1 = moveCol (+ 1) t
      | otherwise = t & cursorState . wrapNext .~ True

addNewline firstCol = doScroll >>> moveCursor
  where
    doScroll t
      | t ^. cursorRow == t ^. scrollBottom = termScrollUp (t ^. scrollTop) 1 t
      | otherwise = t
    moveCursor t = cursorMoveTo (newRow, if firstCol then 0 else t ^. cursorCol) t
      where
        newRow = if t ^. cursorRow == t ^. scrollBottom then t ^. cursorRow else t ^. cursorRow + 1

clearRegion (r1, c1) (r2, c2) t = foldl' (\t' r -> clearRow r c1' c2' t') t [r1' .. r2']
  where
    r1' = limit 0 (t ^. numRows - 1) (min r1 r2)
    r2' = limit 0 (t ^. numRows - 1) (max r1 r2)
    c1' = limit 0 (t ^. numCols - 1) (min c1 c2)
    c2' = limit 0 (t ^. numCols - 1) (max c1 c2)

clearRow row c1 c2 t = activeScreen . tlIndex row . lineCells %~ splice $ t
  where
    splice cells = V.take c1 cells <> V.replicate (c2 - c1 + 1) (' ', t ^. termAttrs) <> V.drop (c2 + 1) cells

limit lo hi = max lo . min hi

between (lo, hi) x = lo <= x && x <= hi

ignoreExc = (`catch` \(_ :: SomeException) -> pure ())

showT :: (Show a) => a -> Text
showT = T.pack . show

sockPath = "/tmp/specter.sock"

data Terminal = Terminal
  { termPty :: Pty,
    termPh :: ProcessHandle,
    termTerm :: TVar Term,
    termParseState :: TVar Text,
    termByteBuffer :: TVar ByteString,
    termTitle :: TVar Text
  }

splitUtf8 bs
  | BS.null bs || len - start <= utf8Len (BS.index bs start) = (bs, BS.empty)
  | otherwise = BS.splitAt start bs
  where
    len = BS.length bs
    start = until (\i -> i <= 0 || BS.index bs i .&. 0xC0 /= 0x80) (subtract 1) (len - 1)
    utf8Len b = if | b .&. 0x80 == 0 -> 1 | b .&. 0xE0 == 0xC0 -> 2 | b .&. 0xF0 == 0xE0 -> 3 | b .&. 0xF8 == 0xF0 -> 4 | otherwise -> 1

data Env = Env
  { envTerminals :: TVar (M.Map Word64 Terminal),
    envCurrent :: TVar (Maybe Word64)
  }

lowestAvailable m = until (`M.notMember` m) (+1) 0

data Request = Request (Maybe Value) Text (Maybe Value)

instance FromJSON Request where
  parseJSON = withObject "Request" \v -> Request <$> v .:? "id" <*> v .: "method" <*> v .:? "params"

respond rid result = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result]

tools =
  toJSON
    [ tool "spawn_terminal" "Spawn terminal with command (default: $SHELL, 160x40). Returns terminal id." [("cmd", "string", False), ("width", "integer", False), ("height", "integer", False)],
      tool "close_terminal" "Close terminal and free its ID" [("terminal", "integer", False)],
      tool "focus_terminal" "Switch current terminal" [("terminal", "integer", True)],
      tool "list_terminals" "List terminal IDs" [],
      tool "read" "Read terminal viewport exactly as displayed, with ANSI color codes preserved. Use Shift+PageUp/Down sequences to scroll." [("terminal", "integer", False)],
      tool "write" "Send input to terminal. Use \\r for Enter, \\u0003 for Ctrl+C, \\u001b for Escape. Double backslashes are halved: \\\\u001b becomes ESC byte." [("terminal", "integer", False), ("input", "string", True)],
      tool "signal" "Send signal: int (SIGINT), term (SIGTERM), kill (SIGKILL)" [("terminal", "integer", False), ("signal", "string", True)],
      tool "resize" "Resize PTY + SIGWINCH" [("terminal", "integer", False), ("width", "integer", True), ("height", "integer", True)]
    ]
  where
    tool :: Text -> Text -> [(Key, Text, Bool)] -> Value
    tool n d ps =
      object
        [ "name" .= n,
          "description" .= d,
          "inputSchema"
            .= object
              [ "type" .= ("object" :: Text),
                "properties" .= object [(p, object ["type" .= t]) | (p, t, _) <- ps],
                "required" .= [p | (p, _, True) <- ps]
              ]
        ]

ok :: Text -> Value
ok t = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= t]]]

err :: Text -> Value
err t = object ["isError" .= True, "content" .= [object ["type" .= ("text" :: Text), "text" .= t]]]

tryIO = fmap (either (err . T.pack . show) id) . E.try @SomeException

param :: (FromJSON a) => Text -> Value -> Maybe a
param k (Object o) = parseMaybe (.: Data.Aeson.Key.fromText k) o
param _ _ = Nothing

spawnTerminal' Env {..} mCmd (width, height) = do
  shell <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
  let (cmd, args) = maybe (shell, []) (\c -> (shell, ["-c", c])) mCmd
  baseEnv <- getEnvironment
  let penv = [("COLUMNS", show width), ("LINES", show height), ("TERM", "xterm-256color")]
           ++ filter ((`notElem` ["COLUMNS", "LINES", "TERM"]) . fst) baseEnv
  (pty, ph) <- spawnWithPty (Just penv) True cmd args (width, height)
  termVar <- newTVarIO $ mkTerm (width, height)
  parseVar <- newTVarIO T.empty
  byteVar <- newTVarIO BS.empty
  titleVar <- newTVarIO $ T.pack $ fromMaybe cmd mCmd
  let term = Terminal pty ph termVar parseVar byteVar titleVar
  void $ async $ reader pty term
  tid <- atomically $ do
    terms <- readTVar envTerminals
    let tid = lowestAvailable terms
    writeTVar envTerminals (M.insert tid term terms)
    writeTVar envCurrent (Just tid)
    pure tid
  void $ async $ waitForProcess ph >> ignoreExc (closePty pty)
  pure (tid, term)
  where
    reader pty Terminal {..} = loop where
      loop = (tryReadPty pty >>= either (const $ threadDelay 10000 >> loop) process) `catch` \(_ :: SomeException) -> pure ()
      process bs = do
        atoms <- atomically $ do
          (prevBytes, prevText) <- (,) <$> readTVar termByteBuffer <*> readTVar termParseState
          let (complete, incomplete) = splitUtf8 (prevBytes <> bs)
              (atoms, remaining) = parseAtoms (prevText <> TE.decodeUtf8Lenient complete)
              newTitle = listToMaybe [t | TermAtomEscapeSequence (EscOSC (OSCSetTitle t)) <- atoms]
          writeTVar termByteBuffer incomplete >> writeTVar termParseState remaining
          mapM_ (writeTVar termTitle) newTitle
          atoms <$ modifyTVar' termTerm (resetViewport . flip processTermAtoms atoms)
        when (TermAtomEscapeSequence (EscCSI CSIDA1) `elem` atoms) $ void $ writePty termPty "\ESC[?1;2c"
        loop

parseAtoms t = case parse parseTermAtom t of
  Done rest atom -> first (atom :) $ parseAtoms rest
  Partial k -> case k T.empty of
    Done rest atom -> first (atom :) $ parseAtoms rest
    _ -> ([], t)
  Fail {} -> ([], t)

getTerminal Env {..} mId = atomically $ do
  current <- readTVar envCurrent
  terms <- readTVar envTerminals
  pure $ case mId <|> current of
    Nothing -> Left "no terminal (spawn or specify one)"
    Just i -> maybe (Left "terminal not found") (Right . (i,)) (M.lookup i terms)

withTerminal env mTerm f = getTerminal env mTerm >>= either (pure . err) (f . snd)

getStatus Env {..} = atomically $
  (\t c -> showT (M.size t) <> " terminals, current: " <> maybe "none" showT c)
    <$> readTVar envTerminals <*> readTVar envCurrent

listTerminals Env {..} = readTVarIO envTerminals >>= fmap (ok . T.unlines) . mapM fmt . M.toList
  where fmt (tid, Terminal {..}) = (showT tid <>) . (": " <>) <$> readTVarIO termTitle

readTerminal env mTerm = withTerminal env mTerm \Terminal {..} ->
  ok . renderViewport <$> readTVarIO termTerm

writeTerminal env mTerm input = withTerminal env mTerm \Terminal {..} ->
  atomically (stateTVar termTerm (processInputEsc input)) >>= \case
    "" -> pure $ ok "scrolled"
    ptyInput -> tryIO $ writePty termPty (TE.encodeUtf8 ptyInput) >> pure (ok "sent")

signalTerminal env mTerm sig = withTerminal env mTerm \Terminal {..} ->
  getPid termPh >>= maybe (pure $ err "no pid") \pid ->
    tryIO $ signalProcess s pid >> pure (ok $ "sent " <> sig)
  where s = case T.toLower sig of "int" -> sigINT; "kill" -> sigKILL; _ -> sigTERM

resizeTerminal env mTerm w h = withTerminal env mTerm \Terminal {..} -> do
  atomically $ modifyTVar' termTerm $ \t -> t & numCols .~ w & numRows .~ h & scrollTop .~ 0 & scrollBottom .~ (h - 1)
    & cursorRow %~ min (h - 1) & cursorCol %~ min (w - 1) & activeScreen %~ rs (t ^. termAttrs) & termAlt %~ rs (t ^. termAttrs)
  resizePty termPty (w, h)
  getPid termPh >>= mapM_ (signalProcess sigWINCH)
  pure $ ok $ "resized to " <> showT w <> "x" <> showT h
  where
    rs attrs s = let s' = rl <$> s in if tlLength s' >= h then tlTakeLast h s' else s' <> tlReplicate (h - tlLength s') (blankLineWith w attrs)
      where rl (TermLine cells wrap) = TermLine (if V.length cells >= w then V.take w cells else cells <> V.replicate (w - V.length cells) (' ', attrs)) wrap

(defaultWidth, defaultHeight) = (160, 40)

spawnTerminal env mCmd mWidth mHeight = tryIO $ do
  (tid, _) <- spawnTerminal' env mCmd (fromMaybe defaultWidth mWidth, fromMaybe defaultHeight mHeight)
  pure $ ok $ showT tid

focusTerminal Env {..} i = atomically $ do
  terms <- readTVar envTerminals
  if M.member i terms
    then Just i <$ writeTVar envCurrent (Just i)
    else pure Nothing
  >>= pure . maybe (err "no such terminal") (\tid -> ok $ "focused " <> showT tid)

closeTerminal Env {..} mTid = do
  result <- atomically $ do
    current <- readTVar envCurrent
    let tid = fromMaybe 0 (mTid <|> current)
    stateTVar envTerminals (M.lookup tid &&& M.delete tid) >>= \case
      Nothing -> pure $ Left "no such terminal"
      Just term -> Right (tid, term) <$ modifyTVar' envCurrent (mfilter (/= tid))
  case result of
    Left e -> pure $ err e
    Right (tid, Terminal {..}) -> do
      getPid termPh >>= mapM_ (ignoreExc . signalProcess sigKILL)
      ignoreExc (closePty termPty)
      pure $ ok $ "closed " <> showT tid

handle _ "initialize" _ =
  pure $
    object
      [ "protocolVersion" .= ("2024-11-05" :: Text),
        "capabilities" .= object ["tools" .= object []],
        "serverInfo" .= object ["name" .= ("specter" :: Text), "version" .= ("1.0.0" :: Text)]
      ]
handle _ "notifications/initialized" _ = pure Null
handle _ "tools/list" _ = pure $ object ["tools" .= tools]
handle env "tools/call" (Just p) = call env (fromMaybe "" $ param @Text "name" p) (fromMaybe (object []) $ param "arguments" p)
handle env "_list" _ = ok <$> getStatus env
handle _ m _ = pure $ object ["error" .= object ["code" .= (-32601 :: Int), "message" .= ("unknown: " <> m)]]

call env "spawn_terminal" a = spawnTerminal env (T.unpack <$> param @Text "cmd" a) (param @Int "width" a) (param @Int "height" a)
call env "close_terminal" a = closeTerminal env (param @Word64 "terminal" a)
call env "focus_terminal" a = focusTerminal env (fromMaybe 0 $ param @Word64 "terminal" a)
call env "list_terminals" _ = listTerminals env
call env "read" a = readTerminal env (param @Word64 "terminal" a)
call env "write" a = writeTerminal env (param @Word64 "terminal" a) (fromMaybe "" $ param @Text "input" a)
call env "signal" a = signalTerminal env (param @Word64 "terminal" a) (fromMaybe "term" $ param @Text "signal" a)
call env "resize" a = resizeTerminal env (param @Word64 "terminal" a) (fromMaybe 120 $ param @Int "width" a) (fromMaybe 24 $ param @Int "height" a)
call _ n _ = pure $ err $ "unknown tool: " <> n

handleClient env conn = E.bracket (socketToHandle conn ReadWriteMode) hClose \h -> do
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  let send = withMVar lock . const . BL.hPutStrLn h
      loop = (BL.fromStrict <$> BC8.hGetLine h >>= (either parseErr dispatch . eitherDecode) >> loop) `catch` \(_ :: SomeException) -> pure ()
      parseErr e = send $ encode $ object ["jsonrpc" .= ("2.0" :: Text), "error" .= object ["code" .= (-32700 :: Int), "message" .= e]]
      dispatch (Request Nothing method params) = void $ handle env method params
      dispatch (Request (Just rid) method params) = handle env method params >>= send . respond (Just rid)
  loop

main = do
  doesFileExist sockPath >>= (`when` removeFile sockPath)
  env <- Env <$> newTVarIO M.empty <*> newTVarIO Nothing
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix sockPath)
  listen sock 5
  forever $ forkIO . handleClient env . fst =<< accept sock
