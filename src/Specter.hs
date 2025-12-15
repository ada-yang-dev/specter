{-# LANGUAGE BlockArguments, MultiWayIf, OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow (first, (>>>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Lens hiding ((.=))
import Control.Monad (forever, mfilter, unless, void, when, (<=<))
import Data.Bool (bool)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.Text hiding (try)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Monoid (Endo (..))

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Word (Word8)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Posix.Pty
import System.Process (ProcessHandle, waitForProcess)

import Prelude hiding (takeWhile)

data Attrs = Attrs
  { _attrsFg, _attrsBg, _attrsIntensity, _attrsUnderline :: !Word8
  , _attrsInverse, _attrsItalic, _attrsStrike :: !Bool
  } deriving (Show, Eq, Ord)
makeLenses ''Attrs

type Cell = (Char, Attrs)

blankAttrs = Attrs 7 0 0 0 False False False

data TermLine = TermLine {_lineCells :: !(V.Vector Cell), _lineHasNewline :: !Bool}
  deriving (Show, Eq, Ord)
makeLenses ''TermLine

newtype Lines = Lines (Seq TermLine) deriving (Show, Eq, Ord, Semigroup, Monoid)

_Lines :: Iso' Lines (Seq TermLine)
_Lines = iso (\(Lines s) -> s) Lines

linesLen = Seq.length . (^._Lines)
linesRep n x = x `seq` Lines (Seq.replicate n x)
linesTake n = _Lines %~ Seq.take n
linesTakeLast n (Lines s) = Lines $ Seq.drop (Seq.length s - n) s
linesDrop n = _Lines %~ Seq.drop n

linesAt i = lens g s where
  cl x = max 0 $ min (Seq.length x - 1) i
  g (Lines x) = Seq.index x (cl x)
  s (Lines x) v = v `seq` Lines (Seq.update (cl x) v x)

blankLine w = TermLine (V.replicate w (' ', blankAttrs)) False
blankLineWith w a = TermLine (V.replicate w (' ', a)) False

data CursorState = CursorState {_wrapNext, _origin :: !Bool} deriving (Show, Eq, Ord)
data SavedCursor = SavedCursor {_savedRow, _savedCol :: !Int, _savedAttrs :: !Attrs, _savedOrigin :: !Bool}
  deriving (Show, Eq, Ord)

data Term = Term
  { _termAttrs :: !Attrs
  , _cursorRow, _cursorCol :: !Int
  , _cursorState :: !CursorState
  , _savedCursor :: !SavedCursor
  , _cursorVisible, _modeWrap, _insertMode, _altScreenActive :: !Bool
  , _numCols, _numRows, _scrollTop, _scrollBottom, _viewportOffset :: !Int
  , _scrollBackLines, _termScreen, _termAlt :: !Lines
  } deriving (Show, Eq, Ord)

makeLenses ''CursorState
makeLenses ''SavedCursor
makeLenses ''Term

mkTerm (w, h) = Term blankAttrs 0 0 (CursorState False False) (SavedCursor 0 0 blankAttrs False)
  True True False False w h 0 (h-1) 0 (Lines mempty) scr scr
  where scr = linesRep h (blankLine w)

activeScreen :: Lens' Term Lines
activeScreen = lens g s where
  g t = t ^. if t^.altScreenActive then termAlt else termScreen
  s t v = t & (if t^.altScreenActive then termAlt else termScreen) .~ v

cursorLine :: Lens' Term TermLine
cursorLine = lens g s where
  g t = t ^. activeScreen . linesAt (t^.cursorRow)
  s t v = t & activeScreen . linesAt (t^.cursorRow) .~ v

cursorLineCells = cursorLine . lineCells

vAt i = lens (\v -> v V.! cl v) (\v x -> v V.// [(cl v, x)]) where cl v = clamp 0 (V.length v - 1) i

addScrollBack f = scrollBackLines %~ (linesTakeLast 1000 . f)
scrollViewport d t = t & viewportOffset %~ (max 0 . min (linesLen $ t^.scrollBackLines) . (+ d))

processInputEsc input t = maybe (input, t) ((,) "" . ($ t) . scrollViewport) $ lookup input
  [ ("\ESC[5;2~", t^.numRows), ("\ESC[6;2~", -(t^.numRows))
  , ("\ESC[1;2A", 1), ("\ESC[1;2B", -1)
  , ("\ESC[5;5~", t^.numRows `div` 2), ("\ESC[6;5~", -(t^.numRows `div` 2))
  , ("\ESC[1;2H", maxBound), ("\ESC[1;2~", maxBound)
  , ("\ESC[4;2~", minBound), ("\ESC[1;2F", minBound) ]

renderViewport t = T.concat [renLine r | r <- [0..rows-1]] <> "\ESC[0m" where
  (rows, cols, voff) = (t^.numRows, t^.numCols, t^.viewportOffset)
  (curR, curC) = (t^.cursorRow, t^.cursorCol)
  showCur = voff == 0 && t^.cursorVisible
  (scr, sb) = (t^.activeScreen, t^.scrollBackLines)
  (sbLen, sbSeq) = (linesLen sb, sb^._Lines)

  getRow r = let v = sbLen - voff + r in if
    | v < 0 -> blankLine cols
    | v < sbLen -> Seq.index sbSeq v
    | otherwise -> scr ^. linesAt (v - sbLen)
  cell r c = fromMaybe (' ', blankAttrs) $ (getRow r ^. lineCells) V.!? c
  hasNL r = getRow r ^. lineHasNewline

  withCur r c (ch, a) = if showCur && r == curR && c == curC then (ch, a & attrsInverse %~ not) else (ch, a)

  renLine r = renCells blankAttrs [withCur r c (cell r c) | c <- [0..cols-1]]
    <> if t^.altScreenActive || hasNL r then "\n" else ""

  renCells _ [] = ""
  renCells p ((ch, a):rest) = sgr p a <> T.singleton ch <> renCells a rest

  sgr p c | p == c = "" | otherwise = "\ESC[" <> T.intercalate ";" (filter (not . T.null) codes) <> "m" where
    codes = [intC, italC, ulC, invC, strikeC, fgC, bgC]
    tog l yes no = if c^.l /= p^.l then if c^.l then yes else no else ""
    intC = case (c^.attrsIntensity, p^.attrsIntensity) of (0,x)|x/=0->"22"; (1,_)->"1"; (2,_)->"2"; _->""
    italC = tog attrsItalic "3" "23"
    ulC = case (c^.attrsUnderline, p^.attrsUnderline) of (0,x)|x/=0->"24"; (1,_)->"4"; (2,_)->"21"; _->""
    invC = tog attrsInverse "7" "27"
    strikeC = tog attrsStrike "9" "29"
    eff x = if x^.attrsInverse then (x^.attrsBg, x^.attrsFg) else (x^.attrsFg, x^.attrsBg)
    (fg, bg) = eff c; (pfg, pbg) = eff p
    fgC = if fg == pfg then "" else "38;5;" <> showT fg
    bgC = if bg == pbg then "" else "48;5;" <> showT bg

data DECMode = DECOM | DECAWM | DECTCEM | AltScreen | AltScreenSaveCursor deriving (Show, Eq, Ord)

decMode = (`lookup` [(6,DECOM),(7,DECAWM),(25,DECTCEM),(47,AltScreen),(1047,AltScreen),(1049,AltScreenSaveCursor)])

data Atom = AChar !Char | ASCF !SCF | AEsc !Esc | AUnk !Text deriving (Show, Eq)
data SCF = Bell | BS | CR | LF | Tab deriving (Show, Eq, Ord, Enum, Bounded)
data Esc = ERI | ERIS | EDECSC | EDECRC | EDECPAM | EDECPNM | ECSI !CSI deriving (Show, Eq)

data CSI
  = CUp !Int | CDown !Int | CFwd !Int | CBack !Int | CPos !Int !Int | CCol !Int | CRow !Int
  | CEL !ELMode | CED !EDMode | CICH !Int | CIL !Int | CDCH !Int | CDL !Int
  | CSU !Int | CSD !Int | CECH !Int | CSoftReset | CSTBM !(Maybe Int) !(Maybe Int)
  | CDECSet !DECMode | CDECRst !DECMode | CSGR ![SGR] | CDSR !Int | CDA1 | CSetMode !Int | CRstMode !Int
  deriving (Show, Eq)

data ELMode = ELToEnd | ELToStart | ELAll deriving (Show, Eq, Ord, Enum, Bounded)
data EDMode = EDBelow | EDAbove | EDAll | EDSaved deriving (Show, Eq, Ord, Enum, Bounded)

data SGR = SReset | SBold | SFaint | SItal | SNoItal | SUL | SDUL | SInv | SNoInv
  | SStrike | SNoStrike | SNorm | SNoUL | SFg !Word8 | SBg !Word8 deriving (Show, Eq)

parseAtom = (AChar <$> satisfy (not . isCtrl)) <|> parseCtrl

parseCtrl = anyChar >>= \case
  '\ESC' -> parseEsc
  c -> pure $ maybe (AUnk $ T.singleton c) ASCF $ lookup c
    [('\a',Bell),('\b',BS),('\r',CR),('\n',LF),('\t',Tab),('\f',LF),('\v',LF)]

parseEsc = anyChar >>= \case
  '[' -> parseCsi
  ']' -> parseOsc
  c -> pure $ maybe (AUnk $ "\ESC" <> T.singleton c) AEsc $
    lookup c [('7',EDECSC),('8',EDECRC),('M',ERI),('c',ERIS),('=',EDECPAM),('>',EDECPNM)]

parseCsi = do
  s <- takeTill (between (0x40, 0x7E) . fromEnum)
  c <- anyChar
  pure $ maybe (AUnk $ "\ESC[" <> s <> T.singleton c) (AEsc . ECSI) $ do
    (priv, args, m) <- either (const Nothing) Just $ parseOnly csiP (s <> T.singleton c)
    (if priv then privCsi else stdCsi) m args
  where
    csiP = do
      priv <- option False (True <$ char '?')
      c <- peekChar'
      args <- if isDigit c || c == ';' then sepBy (option 0 decimal) (char ';') else pure []
      m <- anyChar
      pure (priv, fromMaybe (0:|[]) $ NE.nonEmpty args, m)

arg1 = max 1 . NE.head
argN n (x:|xs) = fromMaybe 0 $ listToMaybe $ drop n (x:xs)
noZero = mfilter (/= 0) . Just

stdCsi 'A' a = Just $ CUp (arg1 a)
stdCsi 'B' a = Just $ CDown (arg1 a)
stdCsi 'C' a = Just $ CFwd (arg1 a)
stdCsi 'D' a = Just $ CBack (arg1 a)
stdCsi 'H' a = Just $ CPos (max 1 $ NE.head a) (max 1 $ argN 1 a)
stdCsi 'f' a = Just $ CPos (max 1 $ NE.head a) (max 1 $ argN 1 a)
stdCsi 'G' a = Just $ CCol (arg1 a)
stdCsi 'd' a = Just $ CRow (arg1 a)
stdCsi 'K' a = CEL <$> [ELToEnd, ELToStart, ELAll] ^? ix (NE.head a)
stdCsi 'J' a = CED <$> [EDBelow, EDAbove, EDAll, EDSaved] ^? ix (NE.head a)
stdCsi '@' a = Just $ CICH (arg1 a)
stdCsi 'L' a = Just $ CIL (arg1 a)
stdCsi 'P' a = Just $ CDCH (arg1 a)
stdCsi 'M' a = Just $ CDL (arg1 a)
stdCsi 'S' a = Just $ CSU (arg1 a)
stdCsi 'T' a = Just $ CSD (arg1 a)
stdCsi 'X' a = Just $ CECH (arg1 a)
stdCsi 'r' a = Just $ CSTBM (noZero $ NE.head a) (noZero $ argN 1 a)
stdCsi 'h' a = Just $ CSetMode (NE.head a)
stdCsi 'l' a = Just $ CRstMode (NE.head a)
stdCsi 'n' a = Just $ CDSR (NE.head a)
stdCsi 'c' _ = Just CDA1
stdCsi 'm' a = Just $ CSGR (parseSGR $ NE.toList a)
stdCsi _ _ = Nothing

privCsi 'h' a = CDECSet <$> decMode (arg1 a)
privCsi 'l' a = CDECRst <$> decMode (arg1 a)
privCsi _ _ = Nothing

parseOsc = do
  s <- T.take 66 <$> takeTill (< ' ')
  _ <- option ' ' (char '\a' <|> (string "\ESC\\" >> pure ' '))
  pure $ AUnk $ "\ESC]" <> s  -- ignore OSC (window title, etc)

parseSGR = \case
  [] -> [SReset]
  38:5:n:r -> SFg (fromIntegral $ clamp 0 255 n) : parseSGR r
  48:5:n:r -> SBg (fromIntegral $ clamp 0 255 n) : parseSGR r
  38:2:_:_:_:r -> parseSGR r
  48:2:_:_:_:r -> parseSGR r
  c:r -> maybe id (:) (sgrCode c) $ parseSGR r
  where
    sgrCode c = lookup c basic <|> fgC c <|> bgC c
    basic = [(0,SReset),(1,SBold),(2,SFaint),(3,SItal),(4,SUL),(7,SInv),(9,SStrike)
            ,(21,SDUL),(22,SNorm),(23,SNoItal),(24,SNoUL),(27,SNoInv),(29,SNoStrike),(39,SFg 7),(49,SBg 0)]
    fgC c | between (30,37) c = Just $ SFg $ fromIntegral $ c - 30
          | between (90,97) c = Just $ SFg $ fromIntegral $ c - 82
          | otherwise = Nothing
    bgC c | between (40,47) c = Just $ SBg $ fromIntegral $ c - 40
          | between (100,107) c = Just $ SBg $ fromIntegral $ c - 92
          | otherwise = Nothing

isCtrl c = fromEnum c <= 0x1F || c == '\DEL'

processAtoms :: Term -> [Atom] -> Term
processAtoms = foldl' (flip processAtom)

processAtom = \case
  AChar c -> procChar c
  ASCF f -> procSCF f
  AEsc e -> procEsc e
  AUnk _ -> id

procSCF = \case
  Bell -> id; BS -> moveCol (subtract 1); CR -> cursorCol .~ 0; LF -> procLF
  Tab -> \t -> t & cursorCol %~ min (t^.numCols - 1) . \c -> ((c+8)`div`8)*8

procEsc = \case
  ERI -> revIdx; ERIS -> resetT; EDECSC -> saveCur; EDECRC -> restoreCur
  ECSI csi -> procCSI csi; _ -> id

saveCur t = t & savedCursor .~ SavedCursor (t^.cursorRow) (t^.cursorCol) (t^.termAttrs) (t^.cursorState.origin)
restoreCur t = t & cursorRow .~ sc^.savedRow & cursorCol .~ sc^.savedCol
  & termAttrs .~ sc^.savedAttrs & cursorState.origin .~ sc^.savedOrigin where sc = t^.savedCursor

procCSI = \case
  CUp n -> moveRow (subtract n); CDown n -> moveRow (+n)
  CFwd n -> moveCol (+n); CBack n -> moveCol (subtract n)
  CPos r c -> curAbsTo (r-1, c-1); CCol c -> moveCol $ const (c-1); CRow r -> setRowAbs (r-1)
  CEL p -> eraseEL p; CED p -> eraseED p
  CICH n -> insChars n; CIL n -> insLines n; CDCH n -> delChars n; CDL n -> delLines n
  CSU n -> scrollFrom scrollUp n; CSD n -> scrollFrom scrollDown n
  CECH n -> eraseChars n; CSoftReset -> resetT
  CSTBM t b -> setSTBM t b >>> curAbsTo (0,0)
  CDECSet m -> procDEC True m; CDECRst m -> procDEC False m
  CSGR sgrs -> termAttrs %~ appEndo (foldMap (Endo . applySGR) sgrs)
  CSetMode 4 -> insertMode .~ True; CRstMode 4 -> insertMode .~ False
  _ -> id

moveRow f t = curTo (f (t^.cursorRow), t^.cursorCol) t
moveCol f t = curTo (t^.cursorRow, f (t^.cursorCol)) t
setRowAbs r t = curAbsTo (r, t^.cursorCol) t
scrollFrom scr n t = scr (t^.scrollTop) n t
resetT t = mkTerm (t^.numCols, t^.numRows)

procDEC on = \case
  DECOM -> (cursorState.origin .~ on) >>> curAbsTo (0,0)
  DECAWM -> modeWrap .~ on
  DECTCEM -> cursorVisible .~ on
  AltScreen | on -> (altScreenActive .~ True) >>> clearAlt | otherwise -> altScreenActive .~ False
  AltScreenSaveCursor | on -> saveCur >>> (altScreenActive .~ True) >>> clearAlt
                      | otherwise -> (altScreenActive .~ False) >>> restoreCur
  where clearAlt t = t & termAlt .~ linesRep (t^.numRows) (blankLine (t^.numCols))

applySGR = \case
  SReset -> const blankAttrs; SBold -> attrsIntensity .~ 1; SFaint -> attrsIntensity .~ 2
  SItal -> attrsItalic .~ True; SNoItal -> attrsItalic .~ False
  SUL -> attrsUnderline .~ 1; SDUL -> attrsUnderline .~ 2; SNoUL -> attrsUnderline .~ 0
  SInv -> attrsInverse .~ True; SNoInv -> attrsInverse .~ False
  SStrike -> attrsStrike .~ True; SNoStrike -> attrsStrike .~ False
  SNorm -> attrsIntensity .~ 0; SFg c -> attrsFg .~ c; SBg c -> attrsBg .~ c

curAbsTo (r, c) t = curTo (r + if t^.cursorState.origin then t^.scrollTop else 0, c) t

curTo (r, c) t = t & cursorRow .~ clamp minY maxY r & cursorCol .~ clamp 0 (t^.numCols-1) c
  & cursorState.wrapNext .~ False
  where (minY, maxY) = if t^.cursorState.origin then (t^.scrollTop, t^.scrollBottom) else (0, t^.numRows-1)

procLF = (cursorLine.lineHasNewline .~ True) >>> addNL True

revIdx t = if t^.cursorRow == t^.scrollTop then scrollDown (t^.scrollTop) 1 t else moveRow (subtract 1) t

eraseEL p t = clearRgn (r, c1) (r, c2) t where
  (r, c) = (t^.cursorRow, t^.cursorCol)
  (c1, c2) = case p of ELToEnd -> (c, t^.numCols-1); ELToStart -> (0, c); ELAll -> (0, t^.numCols-1)

eraseChars n t = clearRgn (r, c) (r, c+n-1) t where (r, c) = (t^.cursorRow, t^.cursorCol)

eraseED = \case
  EDAbove -> \t -> clearRgn (0, 0) (t^.cursorRow, t^.cursorCol) t
  EDBelow -> \t -> clearRgn (t^.cursorRow, t^.cursorCol) (t^.numRows-1, t^.numCols-1) t
  EDAll -> \t -> clearRgn (0, 0) (t^.numRows-1, t^.numCols-1) t
  EDSaved -> scrollBackLines .~ Lines mempty

insChars n t = t & cursorLineCells %~ \cs -> V.take col cs <> V.replicate n' (' ', t^.termAttrs)
  <> V.slice col (t^.numCols - col - n') cs
  where col = t^.cursorCol; n' = clamp 0 (t^.numCols - col) n

insLines n t | between (t^.scrollTop, t^.scrollBottom) (t^.cursorRow) = scrollDown (t^.cursorRow) n t | otherwise = t

delChars n t = t & cursorLineCells %~ \cs -> V.take col cs <> V.slice (col+n') (t^.numCols - col - n') cs
  <> V.replicate n' (' ', t^.termAttrs)
  where col = t^.cursorCol; n' = clamp 0 (t^.numCols - col) n

delLines n t | between (t^.scrollTop, t^.scrollBottom) (t^.cursorRow) = scrollUp (t^.cursorRow) n t | otherwise = t

setSTBM mbT mbB t = t & scrollTop .~ top & scrollBottom .~ bot where
  t' = maybe 0 (subtract 1) mbT; b' = maybe (t^.numRows - 1) (subtract 1) mbB
  top = clamp 0 (t^.numRows-1) (min t' b'); bot = clamp 0 (t^.numRows-1) (max t' b')

scrollDown orig n t = t & activeScreen %~ upd where
  n' = clamp 0 (t^.scrollBottom - orig + 1) n
  blank = blankLineWith (t^.numCols) (t^.termAttrs)
  upd ls = linesTake orig ls <> linesRep n' blank <> linesTake (t^.scrollBottom - orig - n' + 1) (linesDrop orig ls)
    <> linesDrop (t^.scrollBottom + 1) ls

scrollUp orig n t = (copySB >>> activeScreen %~ upd) t where
  n' = clamp 0 (t^.scrollBottom - orig + 1) n
  blank = blankLineWith (t^.numCols) (t^.termAttrs)
  copySB = if not (t^.altScreenActive) && orig == 0 then addScrollBack (linesTake n' (t^.termScreen) <>) else id
  upd ls = linesTake orig ls <> linesTake (t^.scrollBottom - orig - n' + 1) (linesDrop (orig + n') ls)
    <> linesRep n' blank <> linesDrop (t^.scrollBottom + 1) ls

procChar c = moveBefore >>> shift >>> setC >>> moveAfter where
  moveBefore t | t^.modeWrap && t^.cursorState.wrapNext = addNL True t | otherwise = t
  shift t | t^.insertMode && t^.cursorCol < t^.numCols - 1 =
      t & cursorLineCells %~ \cs -> V.take (t^.numCols) (V.take (t^.cursorCol) cs <> V.singleton (' ', blankAttrs) <> V.drop (t^.cursorCol) cs)
    | otherwise = t
  setC t = t & cursorLineCells . vAt (t^.cursorCol) .~ (c, t^.termAttrs)
  moveAfter t | t^.cursorCol < t^.numCols - 1 = moveCol (+1) t | otherwise = t & cursorState.wrapNext .~ True

addNL firstCol = doScr >>> moveCur where
  doScr t = if t^.cursorRow == t^.scrollBottom then scrollUp (t^.scrollTop) 1 t else t
  moveCur t = curTo (if t^.cursorRow == t^.scrollBottom then t^.cursorRow else t^.cursorRow + 1
                    , if firstCol then 0 else t^.cursorCol) t

clearRgn (r1, c1) (r2, c2) t = foldl' (\t' r -> clearRow r c1' c2' t') t [r1'..r2'] where
  r1' = clamp 0 (t^.numRows-1) (min r1 r2); r2' = clamp 0 (t^.numRows-1) (max r1 r2)
  c1' = clamp 0 (t^.numCols-1) (min c1 c2); c2' = clamp 0 (t^.numCols-1) (max c1 c2)

clearRow row c1 c2 t = t & activeScreen . linesAt row . lineCells %~
  \cs -> V.take c1 cs <> V.replicate (c2 - c1 + 1) (' ', t^.termAttrs) <> V.drop (c2 + 1) cs

clamp lo hi = max lo . min hi
between (lo, hi) x = lo <= x && x <= hi

showT :: Show a => a -> Text
showT = T.pack . show

data Terminal = Terminal
  { _tPty :: Pty, _tPh :: ProcessHandle, _tTerm :: TVar Term
  , _tParse :: TVar Text, _tBytes :: TVar BS.ByteString }
makeLenses ''Terminal

newtype Env = Env (TVar Terminal)

splitUtf8 bs
  | BS.null bs || len - start >= utf8Len (BS.index bs start) = (bs, BS.empty)
  | otherwise = BS.splitAt start bs
  where
    len = BS.length bs
    start = until (\i -> i <= 0 || BS.index bs i .&. 0xC0 /= 0x80) (subtract 1) (len - 1)
    utf8Len b = if | b .&. 0x80 == 0 -> 1 | b .&. 0xE0 == 0xC0 -> 2
                   | b .&. 0xF0 == 0xE0 -> 3 | b .&. 0xF8 == 0xF0 -> 4 | otherwise -> 1

spawnPty' cmd (w, h) = do
  penv <- (++ [("COLUMNS", show w), ("LINES", show h), ("TERM", "xterm-256color")])
    . filter ((`notElem` ["COLUMNS","LINES","TERM"]) . fst) <$> getEnvironment
  (pty, ph) <- spawnWithPty (Just penv) True cmd [] (w, h)
  term <- Terminal pty ph <$> newTVarIO (mkTerm (w, h)) <*> newTVarIO "" <*> newTVarIO BS.empty
  term <$ async (ptyReader term)

ptyReader Terminal{..} = go `catch` \(_ :: SomeException) -> pure () where
  go = readPty _tPty >>= proc >> go
  proc bs = do
    atoms <- atomically $ do
      (prevB, prevT) <- (,) <$> readTVar _tBytes <*> readTVar _tParse
      let (complete, incomplete) = splitUtf8 (prevB <> bs)
          (atoms, remaining) = runParser (prevT <> TE.decodeUtf8Lenient complete)
      writeTVar _tBytes incomplete >> writeTVar _tParse remaining
      atoms <$ modifyTVar' _tTerm ((viewportOffset .~ 0) . flip processAtoms atoms)
    when (AEsc (ECSI CDA1) `elem` atoms) $ void $ writePty _tPty "\ESC[?1;2c"

runParser t = case parse parseAtom t of
  Done r a -> first (a:) $ runParser r
  Partial k -> case k "" of Done r a -> first (a:) $ runParser r; _ -> ([], t)
  Fail{} -> ([], t)

readViewport (Env tv) = renderViewport <$> (readTVarIO tv >>= readTVarIO . _tTerm)

sendKeys (Env tv) input = readTVarIO tv >>= \term ->
  atomically (stateTVar (_tTerm term) (processInputEsc $ decodeEscapes input)) >>=
    \case "" -> pure (); i -> void $ writePty (_tPty term) (TE.encodeUtf8 i)

decodeEscapes = T.pack . go . T.unpack where
  go = \case
    '\\':'r':r -> '\r' : go r; '\\':'n':r -> '\n' : go r; '\\':'t':r -> '\t' : go r
    '\\':'\\':r -> '\\' : go r
    '\\':'x':a:b:r | all isHexDigit [a,b] -> chr (digitToInt a * 16 + digitToInt b) : go r
    c:r -> c : go r; [] -> []

-- Agent

cfgModel = "claude-sonnet-4-20250514" :: Text
cfgApiUrl = "https://api.anthropic.com/v1/messages" :: String
cfgApiVersion = "2023-06-01" :: BS.ByteString
cfgThinkingBudget = 16000 :: Int
cfgMaxTokens = 32000 :: Int
cfgTimeout = 300000000 :: Int
cfgPollDelay = 250000 :: Int
cfgContextLimit = 600000 :: Int
cfgCompactTarget = 400000 :: Int

data AgentSt = AgentSt {_stMem, _stThink :: Text}
makeLenses ''AgentSt

sysPrompt msg = T.unlines ["<specter-system>"
  , "You are using specter, a terminal emulator. The viewport below shows what a human would see."
  , "To send input to the terminal, you MUST call the specter_emit tool. Text responses do not affect the terminal."
  , "Your prior responses appear in specter-thinking; long context is summarized into specter-memory."
  , "</specter-system>", "", "<specter-message>", msg, "</specter-message>"]

emitTool = object ["name" .= t "specter_emit", "description" .= t "Send escape sequences to the PTY"
  , "input_schema" .= object ["type" .= t "object", "required" .= [t "keys"]
    , "properties" .= object ["keys" .= object ["type" .= t "string"]]]] where t = id @Text

agentLoop env (mgr, key, msg) stRef = forever step `catch` \(_ :: SomeException) -> pure () where
  wrap tag = maybe "" (\t -> "<specter-" <> tag <> ">\n" <> t <> "\n</specter-" <> tag <> ">") . mfilter (not . T.null) . Just
  prompt st vp = T.unlines $ filter (not . T.null) [wrap "memory" $ st^.stMem, wrap "thinking" $ st^.stThink
    , "<specter-terminal>", vp, "</specter-terminal>"]
  api = fmap (decode . HTTP.responseBody) . (HTTP.httpLbs ?? mgr) <=< mkReq . encode
  mkReq body = HTTP.parseRequest cfgApiUrl <&> \r -> r { HTTP.method = "POST"
    , HTTP.requestBody = HTTP.RequestBodyLBS body, HTTP.responseTimeout = HTTP.responseTimeoutMicro cfgTimeout
    , HTTP.requestHeaders = [("x-api-key", TE.encodeUtf8 key), ("anthropic-version", cfgApiVersion), ("content-type", "application/json")] }
  thinking = ["thinking" .= object ["type" .= t "enabled", "budget_tokens" .= cfgThinkingBudget]] where t = id @Text
  ask = api . object . (["model" .= cfgModel, "max_tokens" .= cfgMaxTokens, "system" .= sysPrompt msg, "tools" .= [emitTool]] ++) . (thinking ++)
  compact vp st = TIO.putStrLn "\x1b[33m[compacting...]\x1b[0m" >>
    api (object ["model" .= cfgModel, "max_tokens" .= cfgMaxTokens, "messages" .= [object ["role" .= t "user", "content" .= ctx]]]) <&>
    \r -> let mem' = fromMaybe (st^.stMem) (r >>= parseText)
              fixed = T.length (sysPrompt msg) + T.length (prompt (AgentSt mem' "") vp)
              cap = cfgCompactTarget - fixed
          in st & stMem .~ mem' & stThink %~ T.takeEnd (max 0 cap) where
      t = id @Text; ctx = T.unlines ["<context>", sysPrompt msg, prompt st vp, "</context>", "Summarize into compact memory."]
  step = threadDelay cfgPollDelay >> readViewport env >>= \vp -> do
    TIO.putStrLn "--- TERMINAL ---" >> TIO.putStrLn vp
    st <- readIORef stRef
    (text, keys) <- fromMaybe ("", "") . (>>= parseResp) <$> ask ["messages" .= [object ["role" .= t "user", "content" .= prompt st vp]]]
    TIO.putStrLn $ "--- RESPONSE ---\n" <> text
    unless (T.null keys) $ sendKeys env keys
    let st' = st & stThink %~ (<> (if T.null (st^.stThink) then "" else "\n\n") <> text)
    writeIORef stRef =<< bool (pure st') (compact vp st') (T.length (sysPrompt msg) + T.length (prompt st' vp) > cfgContextLimit)
    where t = id @Text

parseResp v = do
  Array arr <- parseMaybe (.: "content") v
  let blocks = toList arr
  pure ( T.strip $ T.concat [t | Object b <- blocks, String t <- maybeToList $ parseMaybe (.: "text") b]
       , T.concat [k | Object b <- blocks, String "tool_use" <- maybeToList (parseMaybe (.: "type") b)
           , String "specter_emit" <- maybeToList (parseMaybe (.: "name") b)
           , Object inp <- maybeToList (parseMaybe (.: "input") b), String k <- maybeToList (parseMaybe (.: "keys") inp)])

parseText v = do
  Array arr <- parseMaybe (.: "content") v
  pure $ T.strip $ T.concat [t | Object b <- toList arr, String t <- maybeToList $ parseMaybe (.: "text") b]

main = getArgs >>= \case
  cmd : msg -> agentMain cmd (T.unwords $ T.pack <$> msg)
  _ -> TIO.putStrLn "usage: specter <cmd> <message...>"

agentMain cmd msg = do
  mgr <- HTTP.newManager tlsManagerSettings
  key <- T.pack <$> getEnv "ANTHROPIC_API_KEY"
  term <- spawnPty' cmd (80, 24)
  TIO.putStrLn $ "=== specter ===\n" <> msg <> "\n"
  Env <$> newTVarIO term >>= \env -> newIORef (AgentSt "" "") >>=
    async . agentLoop env (mgr, key, msg) >> waitForProcess (_tPh term) >> TIO.putStrLn "program exited"
