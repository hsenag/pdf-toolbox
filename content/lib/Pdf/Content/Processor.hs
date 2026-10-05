{-# LANGUAGE OverloadedStrings #-}

-- | Process content stream operators maintaining graphics state
--
-- It is pretty experimental

module Pdf.Content.Processor
(
  Processor(..),
  GraphicsState(..),
  GlyphDecoder,
  Glyph(..),
  Span(..),
  MarkedContent(..),
  initialGraphicsState,
  mkProcessor,
  processOp
)
where

import Pdf.Core
import Pdf.Core.Object.Util
import Pdf.Core.Util

import Pdf.Content.Ops
import Pdf.Content.Transform
import qualified Pdf.Content.Encoding.PdfDoc as PdfDoc

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import Control.Monad

-- | Given font name and string, it should return list of glyphs
-- and their widths.
--
-- Note: it should not try to position or scale glyphs to user space,
-- bounding boxes should be defined in glyph space.
--
-- Note: glyph width is a distance between the glyph's origin and
-- the next glyph's origin, so it generally can't be calculated
-- from bounding box
--
-- Note: the 'Processor' actually doesn't cares about glyph's
-- bounding box, so you can return anything you want
type GlyphDecoder = Name -> ByteString -> [(Glyph, Double)]

-- | Glyph
data Glyph = Glyph {
  -- | The code as read from content stream
  glyphCode :: Int,
  -- | Top-left corner of glyph's bounding box
  glyphTopLeft :: Vector Double,
  -- | Bottom-right corner of glyph's bounding box
  glyphBottomRight :: Vector Double,
  -- | Text ectracted from the glyph
  glyphText :: Maybe Text
  }
  deriving Show

-- | Graphics state
data GraphicsState = GraphicsState {
  gsInText :: Bool,    -- ^ Indicates that we are inside text object
  gsCurrentTransformMatrix :: Transform Double,
  gsFont :: Maybe Name,
  gsFontSize :: Maybe Double,
  gsTextMatrix :: Transform Double,      -- ^ Defined only inside text object
  gsTextLineMatrix :: Transform Double,  -- ^ Defined only inside text object
  gsTextLeading :: Double,
  gsTextCharSpacing :: Double,
  gsTextWordSpacing :: Double
  }
  deriving Show

-- | Empty graphics state
initialGraphicsState :: GraphicsState
initialGraphicsState = GraphicsState {
  gsInText = False,
  gsCurrentTransformMatrix = identity,
  gsFont = Nothing,
  gsFontSize = Nothing,
  gsTextMatrix = identity,
  gsTextLineMatrix = identity,
  gsTextLeading = 0,
  gsTextCharSpacing = 0,
  gsTextWordSpacing = 0
  }

-- | Glyphs drawn in one shot
data Span = Span
  { spGlyphs :: [Glyph]
  , spFontName :: Name
  }

-- | A marked content section, begun by BMC or BDC and ended by EMC
--
-- A BDC section's properties can give an @ActualText@, which replaces
-- the text of everything drawn inside it. That is how a document says
-- what characters a glyph stands for when its font can't: headless
-- Chrome, for one, gives many of its fonts a ToUnicode map that sends
-- every code to U+0000 and puts the real text in an ActualText around
-- each glyph.
data MarkedContent
  = PlainContent
  -- ^ The section has no ActualText
  | ActualText Text
  -- ^ The section's ActualText, which no glyph has been given yet
  | ActualTextGiven
  -- ^ The section's ActualText has been given to its first glyph, so
  -- any others drawn in it have no text
  deriving (Eq, Show)

-- | Processor maintains graphics state
data Processor = Processor {
  prState :: GraphicsState,
  prStateStack :: [GraphicsState],
  prGlyphDecoder :: GlyphDecoder,
  prSpans :: [Span],
  -- ^ Each element is a list of glyphs, drawn in one shot
  prMarkedContent :: [MarkedContent]
  -- ^ The marked content sections being drawn in, innermost first
  }

-- | Create processor in initial state
mkProcessor :: Processor
mkProcessor = Processor {
  prState = initialGraphicsState,
  prStateStack = [],
  prGlyphDecoder = \_ _ -> [],
  prSpans = mempty,
  prMarkedContent = []
  }

-- | Process one operation
processOp :: Operator -> Processor -> Either String Processor

processOp (Op_q, []) p = Right p {prStateStack = prState p : prStateStack p}
processOp (Op_q, args) _ = Left ("Op_q: wrong number of arguments: "
                                ++ show args)

processOp (Op_Q, []) p =
  case prStateStack p of
    [] -> Left "Op_Q: state is empty"
    (x:xs) -> Right p {prState = x, prStateStack = xs}
processOp (Op_Q, args) _ = Left ("Op_Q: wrong number of arguments: "
                                ++ show args)

processOp (Op_BT, []) p = do
  ensureInTextObject False p
  let gstate = prState p
  return p {prState = gstate {
    gsInText = True,
    gsTextMatrix = identity,
    gsTextLineMatrix = identity
    }}
processOp (Op_BT, args) _ = Left ("Op_BT: wrong number of arguments: "
                                  ++ show args)

processOp (Op_ET, []) p = do
  ensureInTextObject True p
  let gstate = prState p
  return p {prState = gstate {
    gsInText = False
    }}
processOp (Op_ET, args) _ = Left ("Op_ET: wrong number of arguments: "
                                  ++ show args)

processOp (Op_Td, [txo, tyo]) p = do
  ensureInTextObject True p
  tx <- realValue txo `notice` "Td: x should be a real value"
  ty <- realValue tyo `notice` "Td: y should be a real value"
  let gstate = prState p
      tm = translate tx ty $ gsTextLineMatrix gstate
  Right p {prState = gstate {
    gsTextMatrix = tm,
    gsTextLineMatrix = tm
    }}
processOp (Op_Td, args) _ = Left ("Op_Td: wrong number of arguments: "
                                  ++ show args)

processOp (Op_TD, [txo, tyo]) p = do
  l <- realValue tyo `notice` "TD: y should be a real value"
  p' <- processOp (Op_TL, [Number $ Scientific.fromFloatDigits $ negate l]) p
  processOp (Op_Td, [txo, tyo]) p'
processOp (Op_TD, args) _ = Left ("Op_TD: wrong number of arguments: "
                                  ++ show args)

processOp (Op_Tm, [a', b', c', d', e', f']) p = do
  ensureInTextObject True p
  a <- realValue a' `notice` "Tm: a should be a real value"
  b <- realValue b' `notice` "Tm: b should be a real value"
  c <- realValue c' `notice` "Tm: c should be a real value"
  d <- realValue d' `notice` "Tm: d should be a real value"
  e <- realValue e' `notice` "Tm: e should be a real value"
  f <- realValue f' `notice` "Tm: f should be a real value"
  let gstate = prState p
      tm = Transform a b c d e f
  return p {prState = gstate {
    gsTextMatrix = tm,
    gsTextLineMatrix = tm
    }}
processOp (Op_Tm, args) _ = Left ("Op_Tm: wrong number of arguments: "
                                  ++ show args)

processOp (Op_T_star, []) p = do
  ensureInTextObject True p
  let gstate = prState p
      l = gsTextLeading gstate
  processOp (Op_TD, map (Number . Scientific.fromFloatDigits) [0, negate l]) p
processOp (Op_T_star, args) _ = Left ("Op_T_star: wrong number of arguments: "
                                      ++ show args)

processOp (Op_TL, [lo]) p = do
  l <- realValue lo `notice` "TL: l should be a real value"
  let gstate = prState p
  return p {prState = gstate {
    gsTextLeading = l
    }}
processOp (Op_TL, args) _ = Left ("Op_TL: wrong number of arguments: "
                                  ++ show args)

processOp (Op_cm, [a', b', c', d', e', f']) p = do
  a <- realValue a' `notice` "cm: a should be a real value"
  b <- realValue b' `notice` "cm: b should be a real value"
  c <- realValue c' `notice` "cm: c should be a real value"
  d <- realValue d' `notice` "cm: d should be a real value"
  e <- realValue e' `notice` "cm: e should be a real value"
  f <- realValue f' `notice` "cm: f should be a real value"
  let gstate = prState p
      ctm = Transform a b c d e f `multiply` gsCurrentTransformMatrix gstate
  return p {prState = gstate {
    gsCurrentTransformMatrix = ctm
    }}
processOp (Op_cm, args) _ = Left ("Op_cm: wrong number of arguments: "
                                  ++ show args)

processOp (Op_Tf, [fontO, szO]) p = do
  font <- nameValue fontO `notice` "Tf: font should be a name"
  sz <- realValue szO `notice` "Tf: size should be a real value"
  let gstate = prState p
  return p {prState = gstate {
    gsFont = Just font,
    gsFontSize = Just sz
    }}
processOp (Op_Tf, args) _ = Left ("Op_Tf: wrong number of agruments: "
                                  ++ show args)

processOp (Op_Tj, [String str]) p = do
  let gstate = prState p
  fontName <-
    case gsFont gstate of
      Nothing -> Left "Op_Tj: font not set"
      Just fn -> return fn
  fontSize <-
    case gsFontSize gstate of
      Nothing -> Left "Op_Tj: font size not set"
      Just fs -> return fs
  let (tm, glyphs) = positionGlyghs
                       fontSize
                       (gsCurrentTransformMatrix gstate)
                       (gsTextMatrix gstate)
                       (gsTextCharSpacing gstate)
                       (gsTextWordSpacing gstate)
                       (prGlyphDecoder p fontName str)
  let (glyphs', marked) = actualText (prMarkedContent p) [glyphs]
      sp = Span
        { spGlyphs = concat glyphs'
        , spFontName = fontName
        }
  return p {
    prSpans = sp : prSpans p,
    prMarkedContent = marked,
    prState = gstate {
      gsTextMatrix = tm
      }
    }
processOp (Op_Tj, args) _ = Left ("Op_Tj: wrong number of agruments:"
                                  ++ show args)

processOp (Op_TJ, [Array array]) p = do
  let gstate = prState p
  fontName <-
    case gsFont gstate of
      Nothing -> Left "Op_Tj: font not set"
      Just fn -> return fn
  fontSize <-
    case gsFontSize gstate of
      Nothing -> Left "Op_Tj: font size not set"
      Just fs -> return fs
  let (textMatrix, glyphs) = loop (gsTextMatrix gstate) []
                                  (Vector.toList array)
        where
        loop tm res [] = (tm, reverse res)
        loop tm res (String str : rest) =
          let (tm', gs) = positionGlyghs fontSize
                                         (gsCurrentTransformMatrix gstate)
                                         tm
                                         (gsTextCharSpacing gstate)
                                         (gsTextWordSpacing gstate)
                                         (prGlyphDecoder p fontName str)
          in loop tm' (gs : res) rest
        loop tm res (Number n : rest) =
          let d = Scientific.toRealFloat n
          in loop (translate (-d * fontSize / 1000) 0 tm) res rest
        loop tm res (_:rest) = loop tm res rest

  let (glyphs', marked) = actualText (prMarkedContent p) glyphs
      mkSpan gs = Span
        { spGlyphs = gs
        , spFontName = fontName
        }
  return p {
    prSpans = reverse (map mkSpan glyphs') ++ prSpans p,
    prMarkedContent = marked,
    prState = gstate {
      gsTextMatrix = textMatrix
      }
    }
processOp (Op_TJ, args) _ = Left ("Op_TJ: wrong number of agruments:"
                                  ++ show args)

processOp (Op_Tc, [o]) p = do
  spacing <- realValue o `notice` "Tc: spacing should be a real value"
  let gstate = prState p
  return p {
    prState = gstate {
      gsTextCharSpacing = spacing
      }
    }
processOp (Op_Tc, args) _ = Left ("Op_Tc: wrong number of agruments:"
                                  ++ show args)

processOp (Op_Tw, [o]) p = do
  spacing <- realValue o `notice` "Tw: spacing should be a real value"
  let gstate = prState p
  return p {
    prState = gstate {
      gsTextWordSpacing = spacing
      }
    }
processOp (Op_Tw, args) _ = Left ("Op_Tw: wrong number of agruments:"
                                  ++ show args)

processOp (Op_apostrophe, [o]) p = do
  p' <- processOp (Op_T_star, []) p
  processOp (Op_Tj, [o]) p'
processOp (Op_apostrophe, args) _ =
  Left ("Op_apostrophe: wrong number of agruments:" ++ show args)

processOp (Op_BMC, [_]) p =
  return p {prMarkedContent = PlainContent : prMarkedContent p}
processOp (Op_BMC, args) _ = Left ("Op_BMC: wrong number of agruments:"
                                   ++ show args)

-- The properties can also be the name of an entry in the Properties of
-- the resources, which the processor can't see, so only the ActualText
-- of a dictionary given inline is used
processOp (Op_BDC, [_, props]) p =
  let section =
        case props of
          Dict d | Just (String t) <- HashMap.lookup "ActualText" d
            -> ActualText (decodeTextString t)
          _ -> PlainContent
  in return p {prMarkedContent = section : prMarkedContent p}
processOp (Op_BDC, args) _ = Left ("Op_BDC: wrong number of agruments:"
                                   ++ show args)

-- An EMC without a section to end is ignored, rather than failing the
-- whole page
processOp (Op_EMC, []) p =
  return p {prMarkedContent = drop 1 (prMarkedContent p)}
processOp (Op_EMC, args) _ = Left ("Op_EMC: wrong number of agruments:"
                                   ++ show args)

processOp _ p = return p

-- | Give the glyphs drawn in one operator the ActualText of the
-- outermost section that has one, which replaces anything inside it.
--
-- The text goes to the first glyph the section draws, and the rest
-- have none, so that it is extracted once and where the section
-- starts.
actualText :: [MarkedContent] -> [[Glyph]] -> ([[Glyph]], [MarkedContent])
actualText marked gss =
  case break (/= PlainContent) (reverse marked) of
    (_, []) -> (gss, marked)
    (outer, section : inner) ->
      let (gss', section') = give section gss
      in (gss', reverse (outer ++ section' : inner))
  where
  give section [] = ([], section)
  give section ([] : rest) =
    let (rest', section') = give section rest
    in ([] : rest', section')
  give section ((g : gs) : rest) =
    let text = case section of
          ActualText t -> Just t
          _ -> Nothing
        noText g' = g' {glyphText = Nothing}
    in ((g {glyphText = text} : map noText gs) : map (map noText) rest,
        ActualTextGiven)

-- | Decode a text string, which is UTF-16BE if it starts with a byte
-- order mark and PDFDocEncoding otherwise
decodeTextString :: ByteString -> Text
decodeTextString bs =
  case ByteString.stripPrefix "\254\255" bs of
    Just rest -> Text.decodeUtf16BEWith Text.lenientDecode rest
    Nothing -> Text.concat $
      mapMaybe (`Map.lookup` PdfDoc.encoding) (ByteString.unpack bs)

ensureInTextObject :: Bool -> Processor -> Either String ()
ensureInTextObject inText p =
  unless (inText == gsInText (prState p)) $
    Left $ "ensureInTextObject: expected: " ++ show inText ++ ", found: "
          ++ show (gsInText $ prState p)

positionGlyghs :: Double
               -> Transform Double
               -> Transform Double
               -> Double
               -> Double
               -> [(Glyph, Double)]
               -> (Transform Double, [Glyph])
positionGlyghs fontSize ctm textMatrix charSpacing wordSpacing
  = go textMatrix []
  where
  go tm res [] = (tm, reverse res)
  go tm res ((g, width):gs) =
    let g' = g {
          glyphTopLeft = transform (multiply tm ctm) topLeft,
          glyphBottomRight = transform (multiply tm ctm) bottomRight
          }
        topLeft = transform (scale fontSize fontSize) $ glyphTopLeft g
        bottomRight = transform (scale fontSize fontSize) $ glyphBottomRight g
        spacing = charSpacing + case glyphText g of
                                  Just " " -> wordSpacing
                                  _ -> 0
        tm' = translate (width * fontSize + spacing) 0 tm
    in go tm' (g':res) gs
