{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Concurrent (forkIO, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Exception (SomeException, assert, catch)
import Control.Exception qualified as E
import Control.Lens hiding ((.=), (|>))
import Control.Monad (forM, forM_, forever, guard, mfilter, void, when)
import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.Text hiding (try)
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (Endo (..))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
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

tlIndex :: Int -> Lens' (StrictSeq a) a
tlIndex i = lens getter setter
  where
    getter (StrictSeq v) = assert (i >= 0 && i < Seq.length v) $ Seq.index v i
    setter (StrictSeq v) val = assert (i >= 0 && i < Seq.length v) $ val `seq` StrictSeq (Seq.update i val v)

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

mkTerm (width, height) =
  Term
    { _termAttrs = blankAttrs,
      _cursorRow = 0,
      _cursorCol = 0,
      _cursorState = CursorState False False,
      _savedCursor = SavedCursor 0 0 blankAttrs False,
      _cursorVisible = True,
      _modeWrap = True,
      _insertMode = False,
      _altScreenActive = False,
      _numCols = width,
      _numRows = height,
      _scrollTop = 0,
      _scrollBottom = height - 1,
      _scrollBackLines = tlEmpty,
      _viewportOffset = 0,
      _termScreen = tlReplicate height (blankLine width),
      _termAlt = tlReplicate height (blankLine width)
    }

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

vIndex :: Int -> Lens' (V.Vector a) a
vIndex i = lens (V.! i) (\v x -> v V.// [(i, x)])

addScrollBackLines newLines = scrollBackLines %~ ((<> newLines) >>> tlTakeLast 1000)

resetViewport = viewportOffset .~ 0

processInputEsc input term = case input of
  "\ESC[5;2~" -> ("", scrollUp (term ^. numRows) term)
  "\ESC[6;2~" -> ("", scrollDown (term ^. numRows) term)
  "\ESC[1;2A" -> ("", scrollUp 1 term)
  "\ESC[1;2B" -> ("", scrollDown 1 term)
  "\ESC[5;5~" -> ("", scrollUp (term ^. numRows `div` 2) term)
  "\ESC[6;5~" -> ("", scrollDown (term ^. numRows `div` 2) term)
  "\ESC[1;2H" -> ("", scrollToTop term)
  "\ESC[1;2~" -> ("", scrollToTop term)
  "\ESC[4;2~" -> ("", resetViewport term)
  "\ESC[1;2F" -> ("", resetViewport term)
  _ -> (input, term)
  where
    scrollUp delta t = t & viewportOffset .~ min (tlLength (t ^. scrollBackLines)) (t ^. viewportOffset + delta)
    scrollDown delta t = t & viewportOffset .~ max 0 (t ^. viewportOffset - delta)
    scrollToTop t = t & viewportOffset .~ tlLength (t ^. scrollBackLines)

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
        intCode = case cur ^. attrsIntensity of
          0 -> if prev ^. attrsIntensity /= 0 then "22" else ""
          1 -> "1"
          2 -> "2"
          _ -> ""
        italCode
          | cur ^. attrsItalic == prev ^. attrsItalic = ""
          | cur ^. attrsItalic = "3"
          | otherwise = "23"
        ulCode = case cur ^. attrsUnderline of
          0 -> if prev ^. attrsUnderline /= 0 then "24" else ""
          1 -> "4"
          2 -> "21"
          _ -> ""
        invCode
          | cur ^. attrsInverse == prev ^. attrsInverse = ""
          | cur ^. attrsInverse = "7"
          | otherwise = "27"
        strikeCode
          | cur ^. attrsStrike == prev ^. attrsStrike = ""
          | cur ^. attrsStrike = "9"
          | otherwise = "29"
        (fg, bg) = if cur ^. attrsInverse then (cur ^. attrsBg, cur ^. attrsFg) else (cur ^. attrsFg, cur ^. attrsBg)
        (pfg, pbg) = if prev ^. attrsInverse then (prev ^. attrsBg, prev ^. attrsFg) else (prev ^. attrsFg, prev ^. attrsBg)
        fgCode = if fg == pfg then "" else "38;5;" <> showT fg
        bgCode = if bg == pbg then "" else "48;5;" <> showT bg

data DECPrivateMode = DECOM | DECAWM | DECTCEM | AltScreen | AltScreenSaveCursor
  deriving (Show, Eq, Ord)

intToDECPrivateMode 6 = Just DECOM
intToDECPrivateMode 7 = Just DECAWM
intToDECPrivateMode 25 = Just DECTCEM
intToDECPrivateMode 47 = Just AltScreen
intToDECPrivateMode 1047 = Just AltScreen
intToDECPrivateMode 1049 = Just AltScreenSaveCursor
intToDECPrivateMode _ = Nothing

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

singleCharacterFunction = \case
  '\a' -> Just ControlBell
  '\b' -> Just ControlBackspace
  '\r' -> Just ControlCarriageReturn
  '\n' -> Just ControlLineFeed
  '\t' -> Just ControlTab
  '\f' -> Just ControlLineFeed
  '\v' -> Just ControlLineFeed
  _ -> Nothing

parseEscape =
  anyChar >>= \case
    '[' -> parseCsi
    ']' -> parseOsc
    '7' -> pure $ TermAtomEscapeSequence EscDECSC
    '8' -> pure $ TermAtomEscapeSequence EscDECRC
    'M' -> pure $ TermAtomEscapeSequence EscReverseIndex
    'c' -> pure $ TermAtomEscapeSequence EscRIS
    '=' -> pure $ TermAtomEscapeSequence EscDECPAM
    '>' -> pure $ TermAtomEscapeSequence EscDECPNM
    c -> pure $ TermAtomUnknown ("\ESC" <> T.singleton c)

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
      first <- peekChar'
      args <- if isDigit first || first == ';' then sepBy (option 0 decimal) (char ';') else pure []
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
parseStdCsi 'G' args = Just $ CSICursorPosition 0 (arg1 args)
parseStdCsi 'd' args = Just $ CSICursorPosition (arg1 args) 0
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
  str <- T.pack <$> manyTill' anyChar (char '\a' <|> (string "\ESC\\" >> pure ' '))
  pure $ maybe (TermAtomUnknown ("\ESC]" <> str)) (TermAtomEscapeSequence . EscOSC) (processOsc str)

processOsc str = do
  (c, rest) <- T.uncons str
  (';', title) <- T.uncons rest
  OSCSetTitle title <$ guard (c `elem` ("012" :: String))

parseSGRCodes [] = [SGRReset]
parseSGRCodes (0 : rest) = SGRReset : parseSGRCodes rest
parseSGRCodes (1 : rest) = SGRBold : parseSGRCodes rest
parseSGRCodes (2 : rest) = SGRFaint : parseSGRCodes rest
parseSGRCodes (3 : rest) = SGRItalic : parseSGRCodes rest
parseSGRCodes (4 : rest) = SGRUnderline : parseSGRCodes rest
parseSGRCodes (7 : rest) = SGRInverse : parseSGRCodes rest
parseSGRCodes (9 : rest) = SGRStrike : parseSGRCodes rest
parseSGRCodes (21 : rest) = SGRDoubleUnderline : parseSGRCodes rest
parseSGRCodes (22 : rest) = SGRNormal : parseSGRCodes rest
parseSGRCodes (23 : rest) = SGRNoItalic : parseSGRCodes rest
parseSGRCodes (24 : rest) = SGRNoUnderline : parseSGRCodes rest
parseSGRCodes (27 : rest) = SGRNoInverse : parseSGRCodes rest
parseSGRCodes (29 : rest) = SGRNoStrike : parseSGRCodes rest
parseSGRCodes (39 : rest) = SGRFgColor 7 : parseSGRCodes rest
parseSGRCodes (49 : rest) = SGRBgColor 0 : parseSGRCodes rest
parseSGRCodes (c : rest) | c >= 30 && c <= 37 = SGRFgColor (fromIntegral $ c - 30) : parseSGRCodes rest
parseSGRCodes (c : rest) | c >= 90 && c <= 97 = SGRFgColor (fromIntegral $ c - 90 + 8) : parseSGRCodes rest
parseSGRCodes (c : rest) | c >= 40 && c <= 47 = SGRBgColor (fromIntegral $ c - 40) : parseSGRCodes rest
parseSGRCodes (c : rest) | c >= 100 && c <= 107 = SGRBgColor (fromIntegral $ c - 100 + 8) : parseSGRCodes rest
parseSGRCodes (38 : 5 : n : rest) = SGRFgColor (fromIntegral $ max 0 (min 255 n)) : parseSGRCodes rest
parseSGRCodes (48 : 5 : n : rest) = SGRBgColor (fromIntegral $ max 0 (min 255 n)) : parseSGRCodes rest
parseSGRCodes (38 : 2 : _ : _ : _ : rest) = parseSGRCodes rest
parseSGRCodes (48 : 2 : _ : _ : _ : rest) = parseSGRCodes rest
parseSGRCodes (_ : rest) = parseSGRCodes rest

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
  ControlTab -> \t -> t & cursorCol %~ \c -> min (t ^. numCols - 1) (((c + 8) `div` 8) * 8)
  ControlLineFeed -> processLF
  ControlCarriageReturn -> cursorCol .~ 0

processEsc = \case
  EscReverseIndex -> reverseIndex
  EscRIS -> resetTerm
  EscDECSC -> saveCursor
  EscDECRC -> restoreCursor
  EscDECPAM -> id
  EscDECPNM -> id
  EscCSI csi -> processCSI csi
  EscOSC _ -> id

saveCursor t = t & savedCursor .~ SavedCursor (t ^. cursorRow) (t ^. cursorCol) (t ^. termAttrs) (t ^. cursorState . origin)

restoreCursor t =
  t
    & cursorRow .~ sc ^. savedRow
    & cursorCol .~ sc ^. savedCol
    & termAttrs .~ sc ^. savedAttrs
    & cursorState . origin .~ sc ^. savedOrigin
  where
    sc = t ^. savedCursor

processCSI = \case
  CSICursorUp n -> moveRow (subtract n)
  CSICursorDown n -> moveRow (+ n)
  CSICursorForward n -> moveCol (+ n)
  CSICursorBack n -> moveCol (subtract n)
  CSICursorPosition 0 col -> moveCol (const (col - 1))
  CSICursorPosition row 0 -> setRowAbs (row - 1)
  CSICursorPosition row col -> cursorMoveAbsoluteTo (row - 1, col - 1)
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
  CSIDeviceStatusReport _ -> id
  CSIDA1 -> id
  CSISetMode 4 -> insertMode .~ True
  CSISetMode _ -> id
  CSIResetMode 4 -> insertMode .~ False
  CSIResetMode _ -> id

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
    termParseState :: TVar Text
  }

data Workspace = Workspace
  { wsOwned :: TVar (S.Set Word64),
    wsCurrent :: TVar (Maybe Word64)
  }

data LinkedMap k v = LinkedMap !(M.Map k v) !(Seq k)

lmEmpty = LinkedMap M.empty Seq.empty

lmLookup k (LinkedMap m _) = M.lookup k m

lmInsert k v (LinkedMap m s) = LinkedMap (M.insert k v m) (s |> k)

lmDelete k (LinkedMap m s) = LinkedMap (M.delete k m) (Seq.filter (/= k) s)

lmElems (LinkedMap m _) = M.elems m

lmToList (LinkedMap m _) = M.toList m

lmTrim maxSize (LinkedMap m s)
  | Seq.length s <= maxSize = ([], LinkedMap m s)
  | otherwise = (trimmed, LinkedMap m' s')
  where
    excess = Seq.length s - maxSize
    toTrim = toList $ Seq.take excess s
    trimmed = mapMaybe (\k -> (k,) <$> M.lookup k m) toTrim
    m' = foldl' (flip M.delete) m toTrim
    s' = Seq.drop excess s

data Env = Env
  { envTerminals :: TVar (M.Map Word64 Terminal),
    envFloating :: TVar (S.Set Word64),
    envActive :: TVar (M.Map Word64 Workspace),
    envOrphans :: TVar (LinkedMap Word64 Workspace),
    envNextTermId :: TVar Word64,
    envNextWsId :: TVar Word64
  }

maxOrphans = 1024

type ClientAttached = TVar (Maybe Word64)

data Request = Request (Maybe Value) Text (Maybe Value)

instance FromJSON Request where
  parseJSON = withObject "Request" \v -> Request <$> v .:? "id" <*> v .: "method" <*> v .:? "params"

respond rid result = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result]

tools =
  toJSON
    [ tool "attach" "Attach to workspace by ID or create new → {id, created}" [("id", "integer", False)],
      tool "spawn_terminal" "Spawn terminal with command (default: $SHELL, 160x40). Returns terminal id." [("cmd", "string", False), ("width", "integer", False), ("height", "integer", False)],
      tool "focus_terminal" "Switch current terminal" [("terminal", "integer", True)],
      tool "list_terminals" "List terminal IDs in workspace" [],
      tool "read" "Read terminal viewport exactly as displayed, with ANSI color codes preserved. Use Shift+PageUp/Down sequences to scroll." [("terminal", "integer", False)],
      tool "write" "Send input to terminal. Use \\r for Enter, \\u0003 for Ctrl+C, \\u001b for Escape. Double backslashes are halved: \\\\u001b becomes ESC byte." [("terminal", "integer", False), ("input", "string", True)],
      tool "signal" "Send signal: int (SIGINT), term (SIGTERM), kill (SIGKILL)" [("terminal", "integer", False), ("signal", "string", True)],
      tool "resize" "Resize PTY + SIGWINCH" [("terminal", "integer", False), ("width", "integer", True), ("height", "integer", True)],
      tool "float_terminal" "Move terminal to floating pool → id" [("terminal", "integer", True)],
      tool "grab_floating" "Move floating terminal into workspace → id" [("id", "integer", True)]
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

spawnTerminal' Env {..} wsId mCmd (width, height) = do
  cmd <- maybe (fromMaybe "/bin/sh" <$> lookupEnv "SHELL") pure mCmd
  baseEnv <- getEnvironment
  let penv =
        [("COLUMNS", show width), ("LINES", show height), ("TERM", "xterm-256color")]
          ++ filter (\(k, _) -> k `notElem` ["COLUMNS", "LINES", "TERM"]) baseEnv
  (pty, ph) <- spawnWithPty (Just penv) True cmd [] (width, height)
  termVar <- newTVarIO $ mkTerm (width, height)
  parseVar <- newTVarIO T.empty
  let term = Terminal pty ph termVar parseVar
  void $ async $ reader pty term
  tid <- atomically $ do
    tid <- readTVar envNextTermId
    modifyTVar' envNextTermId (+ 1)
    modifyTVar' envTerminals (M.insert tid term)
    mWs <- M.lookup wsId <$> readTVar envActive
    forM_ mWs \Workspace {..} -> do
      modifyTVar' wsOwned (S.insert tid)
      writeTVar wsCurrent (Just tid)
    pure tid
  void $ async $ do
    _ <- waitForProcess ph
    atomically $ cleanupTerminal envTerminals envFloating envActive envOrphans tid
    ignoreExc (closePty pty)
  pure (tid, term)
  where
    reader pty Terminal {..} =
      forever $
        ( tryReadPty pty >>= \case
            Left _ -> threadDelay 10000
            Right bs -> do
              atoms <- atomically $ do
                leftover <- readTVar termParseState
                let input = leftover <> TE.decodeUtf8Lenient bs
                    (atoms, remaining) = parseAtoms input
                writeTVar termParseState remaining
                modifyTVar' termTerm (resetViewport . flip processTermAtoms atoms)
                pure atoms
              forM_ atoms $ \case
                TermAtomEscapeSequence (EscCSI CSIDA1) -> void $ writePty termPty "\ESC[?1;2c"
                _ -> pure ()
        )
          `catch` \(_ :: SomeException) -> threadDelay 100000

cleanupTerminal envTerminals envFloating envActive envOrphans tid = do
  modifyTVar' envTerminals (M.delete tid)
  floating <- readTVar envFloating
  if S.member tid floating
    then modifyTVar' envFloating (S.delete tid)
    else do
      let removeFromWs Workspace {..} = modifyTVar' wsOwned (S.delete tid) >> modifyTVar' wsCurrent (mfilter (/= tid))
      mapM_ removeFromWs . M.elems =<< readTVar envActive
      orphans <- readTVar envOrphans
      emptyWids <- forM (lmToList orphans) \(wid, ws) -> do
        removeFromWs ws
        owned <- readTVar (wsOwned ws)
        pure $ wid <$ guard (S.null owned)
      mapM_ (modifyTVar' envOrphans . lmDelete) (catMaybes emptyWids)

parseAtoms = go []
  where
    go acc t = case parse parseTermAtom t of
      Done rest atom -> go (atom : acc) rest
      Partial k -> case k T.empty of
        Done rest atom -> go (atom : acc) rest
        _ -> (reverse acc, t)
      Fail {} -> (reverse acc, t)

getTerminal Env {..} Workspace {..} mId = atomically $ do
  owned <- readTVar wsOwned
  current <- readTVar wsCurrent
  terms <- readTVar envTerminals
  case mId <|> current of
    Nothing -> pure $ Left "no terminal (spawn or specify one)"
    Just i
      | S.member i owned -> pure $ maybe (Left "terminal not found") (Right . (i,)) (M.lookup i terms)
      | otherwise -> pure $ Left "terminal not owned by workspace"

getOwnedWorkspace Env {..} att = atomically $ do
  readTVar att >>= \case
    Nothing -> pure Nothing
    Just wid -> fmap (wid,) . M.lookup wid <$> readTVar envActive

withWorkspace env att f = getOwnedWorkspace env att >>= maybe (pure $ err "not attached to any workspace") (uncurry f)

withTerminal env att mTerm f = withWorkspace env att \_ ws -> getTerminal env ws mTerm >>= either (pure . err) (f . snd)

getStatus Env {..} = atomically $ do
  active <- readTVar envActive
  orphans <- readTVar envOrphans
  floating <- readTVar envFloating
  activeItems <- forM (M.toList active) \(wid, Workspace {..}) -> do
    owned <- readTVar wsOwned
    pure $ "+" <> showT wid <> " (" <> showT (S.size owned) <> " terminals)"
  orphanItems <- forM (lmToList orphans) \(wid, Workspace {..}) -> do
    owned <- readTVar wsOwned
    pure $ "?" <> showT wid <> " (" <> showT (S.size owned) <> " terminals)"
  let floatItem = ["floating: " <> T.unwords (map showT (S.toList floating)) | not (S.null floating)]
  pure $ case activeItems ++ orphanItems ++ floatItem of
    [] -> "no workspaces"
    items -> T.unlines items

listTerminals env att =
  getOwnedWorkspace env att >>= \case
    Nothing -> pure $ err "not attached to any workspace"
    Just (_, Workspace {..}) -> do
      owned <- readTVarIO wsOwned
      pure $ ok $ T.unwords $ map showT $ S.toList owned

readTerminal env att mTerm = withTerminal env att mTerm \Terminal {..} -> do
  screen <- readTVarIO termTerm
  pure $ ok $ renderViewport screen

writeTerminal env att mTerm input = withTerminal env att mTerm \Terminal {..} -> do
  ptyInput <- atomically $ stateTVar termTerm (processInputEsc input)
  if T.null ptyInput
    then pure $ ok "scrolled"
    else tryIO $ writePty termPty (TE.encodeUtf8 ptyInput) >> pure (ok "sent")

signalTerminal env att mTerm sig = withTerminal env att mTerm \Terminal {..} ->
  getPid termPh >>= maybe (pure $ err "no pid") \pid ->
    tryIO $ signalProcess s pid >> pure (ok $ "sent " <> sig)
  where
    s = case T.toLower sig of "int" -> sigINT; "term" -> sigTERM; "kill" -> sigKILL; _ -> sigTERM

resizeTerminal env att mTerm w h = withTerminal env att mTerm \Terminal {..} -> do
  resizePty termPty (w, h)
  atomically $ modifyTVar' termTerm $ \t ->
    t
      & numCols .~ w
      & numRows .~ h
      & scrollTop .~ 0
      & scrollBottom .~ (h - 1)
      & cursorRow %~ min (h - 1)
      & cursorCol %~ min (w - 1)
      & activeScreen %~ resizeScreen w h (t ^. termAttrs)
      & termAlt %~ resizeScreen w h (t ^. termAttrs)
  getPid termPh >>= mapM_ (signalProcess sigWINCH)
  pure $ ok $ "resized to " <> showT w <> "x" <> showT h
  where
    resizeScreen newW newH attrs oldScreen =
      let blank = blankLineWith newW attrs
          resizeLine (TermLine cells wrapped) =
            let newCells = if V.length cells >= newW then V.take newW cells else cells <> V.replicate (newW - V.length cells) (' ', attrs)
             in TermLine newCells wrapped
       in if tlLength oldScreen >= newH
            then tlTakeLast newH $ fmap resizeLine oldScreen
            else fmap resizeLine oldScreen <> tlReplicate (newH - tlLength oldScreen) blank

(defaultWidth, defaultHeight) = (160, 40)

spawnTerminal env att mCmd mWidth mHeight =
  withWorkspace env att \wsId _ -> tryIO $ do
    (tid, _) <- spawnTerminal' env wsId mCmd (fromMaybe defaultWidth mWidth, fromMaybe defaultHeight mHeight)
    pure $ ok $ showT tid

focusTerminal env att i = withWorkspace env att \_ Workspace {..} -> do
  focused <-
    atomically $
      readTVar wsOwned >>= \owned ->
        if S.member i owned then writeTVar wsCurrent (Just i) >> pure True else pure False
  pure $ if focused then ok $ "focused terminal " <> showT i else err "no such terminal"

floatTerminal env@Env {..} att i = withWorkspace env att \_ Workspace {..} ->
  atomically $ do
    owned <- readTVar wsOwned
    if S.member i owned
      then do
        modifyTVar' wsOwned (S.delete i)
        modifyTVar' wsCurrent (mfilter (/= i))
        modifyTVar' envFloating (S.insert i)
        pure $ ok $ "floated " <> showT i
      else pure $ err "terminal not owned by workspace"

grabFloating env@Env {..} att tid = withWorkspace env att \_ Workspace {..} ->
  atomically $ do
    floating <- readTVar envFloating
    if S.member tid floating
      then do
        modifyTVar' envFloating (S.delete tid)
        modifyTVar' wsOwned (S.insert tid)
        writeTVar wsCurrent (Just tid)
        pure $ ok $ "grabbed terminal " <> showT tid
      else pure $ err "terminal not floating"

attachWorkspace env@Env {..} att mReqId =
  readTVarIO att >>= \case
    Just _ -> pure $ err "already attached to a workspace"
    Nothing -> maybe (createWorkspace env att) tryReconnect mReqId
  where
    tryReconnect reqId = do
      reconnected <-
        atomically $
          readTVar envOrphans >>= \orphans ->
            forM (lmLookup reqId orphans) \ws -> do
              modifyTVar' envOrphans (lmDelete reqId)
              modifyTVar' envActive (M.insert reqId ws)
              writeTVar att (Just reqId)
              pure reqId
      maybe (createWorkspace env att) (\wid -> pure $ okAttach wid False) reconnected

createWorkspace Env {..} att = do
  wid <- atomically $ do
    wid <- readTVar envNextWsId <* modifyTVar' envNextWsId (+ 1)
    ws <- Workspace <$> newTVar S.empty <*> newTVar Nothing
    modifyTVar' envActive (M.insert wid ws)
    writeTVar att (Just wid)
    pure wid
  pure $ okAttach wid True

okAttach wid created =
  object
    [ "content" .= [object ["type" .= ("text" :: Text), "text" .= msg]],
      "workspace" .= wid,
      "created" .= created
    ]
  where
    msg = (if created then "created " else "attached to ") <> showT wid

killTerminal envTerminals tid = do
  mTerm <- atomically $ stateTVar envTerminals \terms -> (M.lookup tid terms, M.delete tid terms)
  forM_ mTerm \Terminal {..} -> do
    getPid termPh >>= mapM_ (ignoreExc . signalProcess sigKILL)
    ignoreExc (closePty termPty)

purgeOrphans Env {..} = do
  orphanWs <- atomically $ stateTVar envOrphans \o -> (lmElems o, lmEmpty)
  forM_ orphanWs \Workspace {..} ->
    mapM_ (killTerminal envTerminals) . S.toList =<< readTVarIO wsOwned
  pure $ ok $ "killed " <> showT (length orphanWs) <> " orphaned workspaces"

handle _ _ "initialize" _ =
  pure $
    object
      [ "protocolVersion" .= ("2024-11-05" :: Text),
        "capabilities" .= object ["tools" .= object []],
        "serverInfo" .= object ["name" .= ("specter" :: Text), "version" .= ("1.0.0" :: Text)]
      ]
handle _ _ "notifications/initialized" _ = pure Null
handle _ _ "tools/list" _ = pure $ object ["tools" .= tools]
handle env att "tools/call" (Just p) = call env att (fromMaybe "" $ param @Text "name" p) (fromMaybe (object []) $ param "arguments" p)
handle env _ "_purge" _ = purgeOrphans env
handle env _ "_list" _ = ok <$> getStatus env
handle _ _ m _ = pure $ object ["error" .= object ["code" .= (-32601 :: Int), "message" .= ("unknown: " <> m)]]

call env att "attach" a = attachWorkspace env att (param @Word64 "id" a)
call env att "spawn_terminal" a = spawnTerminal env att (T.unpack <$> param @Text "cmd" a) (param @Int "width" a) (param @Int "height" a)
call env att "focus_terminal" a = focusTerminal env att (fromMaybe 0 $ param @Word64 "terminal" a)
call env att "list_terminals" _ = listTerminals env att
call env att "read" a = readTerminal env att (param @Word64 "terminal" a)
call env att "write" a = writeTerminal env att (param @Word64 "terminal" a) (fromMaybe "" $ param @Text "input" a)
call env att "signal" a = signalTerminal env att (param @Word64 "terminal" a) (fromMaybe "term" $ param @Text "signal" a)
call env att "resize" a = resizeTerminal env att (param @Word64 "terminal" a) (fromMaybe 120 $ param @Int "width" a) (fromMaybe 24 $ param @Int "height" a)
call env att "float_terminal" a = floatTerminal env att (fromMaybe 0 $ param @Word64 "terminal" a)
call env att "grab_floating" a = grabFloating env att (fromMaybe 0 $ param @Word64 "id" a)
call _ _ n _ = pure $ err $ "unknown tool: " <> n

handleClient env conn = E.bracket (socketToHandle conn ReadWriteMode) hClose \h -> do
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  attached <- newTVarIO Nothing
  let loop = do
        line <- BL.fromStrict <$> BC8.hGetLine h
        case eitherDecode line of
          Left e -> withMVar lock \_ ->
            BL.hPutStrLn h $ encode $ object ["jsonrpc" .= ("2.0" :: Text), "error" .= object ["code" .= (-32700 :: Int), "message" .= e]]
          Right (Request Nothing method params) ->
            void $ handle env attached method params
          Right (Request (Just rid) method params) -> void $ forkIO $ do
            result <- handle env attached method params
            withMVar lock \_ -> BL.hPutStrLn h (respond (Just rid) result)
        loop
  loop `catch` \(_ :: SomeException) -> pure ()
  orphanWorkspaces env attached

orphanWorkspaces env@Env {..} attached =
  readTVarIO attached >>= mapM_ \wid -> do
    let go ws@Workspace {..} = do
          modifyTVar' envActive (M.delete wid)
          owned <- readTVar wsOwned
          if S.null owned
            then pure []
            else do
              orphans <- lmInsert wid ws <$> readTVar envOrphans
              let (trimmed, orphans') = lmTrim maxOrphans orphans
              writeTVar envOrphans orphans'
              pure trimmed
    toTrim <- atomically $ readTVar envActive >>= maybe (pure []) go . M.lookup wid
    mapM_ (killWorkspace env) toTrim

killWorkspace Env {..} (_, Workspace {..}) =
  mapM_ (killTerminal envTerminals) . S.toList =<< readTVarIO wsOwned

main = do
  doesFileExist sockPath >>= (`when` removeFile sockPath)
  env <-
    Env
      <$> newTVarIO M.empty
      <*> newTVarIO S.empty
      <*> newTVarIO M.empty
      <*> newTVarIO lmEmpty
      <*> newTVarIO 0
      <*> newTVarIO 0
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix sockPath) >> listen sock 5
  forever $ accept sock >>= forkIO . handleClient env . fst
