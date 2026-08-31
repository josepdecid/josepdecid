{-# LANGUAGE OverloadedStrings #-}

-- | Renders the stats as a self-contained, animated SVG.
--
-- The card is laid out on a fixed grid rather than by measuring text, so it
-- does not depend on which monospace font the viewer's browser picks: labels
-- and values sit in two columns at known x positions. Only the ASCII panel
-- relies on the font being monospaced, and it is a block of its own.
module ProfileCard.Render
  ( render
  ) where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time          (defaultTimeLocale, formatTime)

import           ProfileCard.Config
import           ProfileCard.Format
import           ProfileCard.Stats

-- Geometry --------------------------------------------------------------

-- The art panel and the statistics column trade width against each other, and
-- 'contentRight' is fixed, so these four move together. For a C-column,
-- R-row picture at type size F:
--
--   art     = C * 0.6154 * F  wide, R * F tall
--   keyX    = asciiX + art + 20
--   barW    = contentRight - keyX, split into three legend columns
--
-- F is then the largest size where the art still clears the window body, the
-- longest value row (~303px) still ends before 'contentRight', and a legend
-- column is still wider than its widest item (~150px). At 58x34 that is 7.
cardW, cardH, titleH, asciiX, asciiSize, asciiLead, keyX, valX, headY, promptSize, ruleY, rowY, rowLead :: Int
cardW     = 840
cardH     = 372
titleH    = 40
asciiX    = 42
asciiSize = 7
-- Leading must equal the type size: the panel is a picture on a character
-- grid, not prose, so rows have to touch exactly.
asciiLead = asciiSize
keyX      = 312
valX      = keyX + 110
headY     = 78
promptSize = 15   -- type size of the prompt line
ruleY     = 90
rowY      = 114
rowLead   = 24

logoX, logoY, logoW :: Int
logoX = 58
logoY = 115
logoW = 150

barX, barY, barW, barH, legendY, legendLead, legendColW :: Int
-- The bar and the legend share the statistics column, so they start where it
-- starts and run to the fixed right edge.
barX       = keyX
barY       = 282
barW       = contentRight - barX
barH       = 10
legendY    = 318
legendLead = 22
legendColW = 162

-- | Right edge of the content column: the rule, the language bar and the build
-- date all finish here. Fixed, so widening the art panel eats into the bar
-- rather than pushing anything off the card.
contentRight :: Int
contentRight = cardW - 42

-- | A run of value text with a role that decides its colour.
data Span = Plain Text | Accent Text | Muted Text | Added Text | Deleted Text

-- Rows ------------------------------------------------------------------

statRows :: Config -> Stats -> [(Text, [Span])]
statRows cfg st =
  [ ("OS",        [Plain (cfgOS cfg)])
  , ("Uptime",    [Plain (uptimeSince (pfCreatedAt p) (stGeneratedAt st))])
  , ("Repos",     [ Accent (commas (stRepos st))
                  , Muted ("  {private: " <> commas (stPrivate st)
                           <> ", contributed to: " <> commas (pfContribTo p) <> "}")
                  ])
  , ("Commits",   [ Accent (commas (stCommits st))
                  , Muted ("  {" <> year (stGeneratedAt st) <> ": "
                           <> commas (stCommitsYear st) <> "}")
                  ])
  , ("Stars",     [Accent (commas (stStars st))])
  , ("Lines",     [ Accent (commas (stLocAdded st - stLocDeleted st))
                  , Muted "  ("
                  , Added ("+" <> commas (stLocAdded st))
                  , Muted ", "
                  , Deleted ("-" <> commas (stLocDeleted st))
                  , Muted ")"
                  ])
  ]
  where
    p = stProfile st
    year t = T.pack (formatTime defaultTimeLocale "%Y" t)

-- Rendering -------------------------------------------------------------

-- | @render config theme stats asciiArt@.
render :: Config -> Theme -> Stats -> Text -> Text
render cfg th st ascii = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> tshow cardW
      <> "\" height=\"" <> tshow cardH <> "\" viewBox=\"0 0 " <> tshow cardW
      <> " " <> tshow cardH <> "\" role=\"img\""
  , "     aria-label=\"" <> escapeXml (cfgTitle cfg) <> " statistics card\">"
  , "<title>" <> escapeXml (cfgTitle cfg) <> "</title>"
  , styleBlock th cs
  , chrome th cfg
  ]
  <> panel th ascii
  <> promptLine th cfg st
  <> concat (zipWith (row th) [0 ..] rows)
  <> caret cs (length rows)
  <> languageBar th st cs
  <> legend st cs
  <> [ "</svg>" ]
  where
    rows = statRows cfg st
    cs   = cascade (length rows) (length (stLangs st))

-- | One shared stylesheet: every animated element just picks a delay. SVG
-- rendered inside an @\<img\>@ cannot run scripts, but CSS animation works.
--
-- The cascade animates @transform@ only, never @opacity@. Anything that fades
-- in from zero is invisible whenever the animation clock does not run — a
-- background tab, a rasteriser, a print stylesheet, an image proxy that
-- snapshots the first frame — and a profile card that renders as an empty
-- window frame is worse than one that does not move. Sliding elements are
-- fully legible at every point in the animation, including its first frame.
styleBlock :: Theme -> Cascade -> Text
styleBlock th cs = T.unlines
  [ "<style>"
  , "  text { font-family: 'JetBrains Mono','Fira Code','SFMono-Regular',"
      <> "ui-monospace,Menlo,Consolas,'Courier New',monospace;"
      <> " font-size: 14px; dominant-baseline: middle; }"
  , "  .ascii { font-size: " <> tshow asciiSize <> "px; fill: " <> thAscii th
      <> "; white-space: pre; }"
  , "  .key { fill: " <> thKey th <> "; }"
  , "  .val { fill: " <> thValue th <> "; }"
  , "  .acc { fill: " <> thAccent th <> "; font-weight: 700; }"
  , "  .add { fill: " <> thAdded th <> "; }"
  , "  .del { fill: " <> thDeleted th <> "; }"
  , "  .mut { fill: " <> thMuted th <> "; font-size: 12px; }"
  , "  .prompt { fill: " <> thPrompt th <> "; font-weight: 700; font-size: "
      <> tshow promptSize <> "px; }"
  , "  .fx { animation: reveal 0.5s cubic-bezier(0.22, 1, 0.36, 1) both; }"
  , "  @keyframes reveal { from { transform: translateX(-14px); }"
      <> " to { transform: translateX(0); } }"
  , "  .cursor { fill: " <> thCursor th <> "; animation: blink 1.1s steps(2, start) "
      <> secs (delayFor (csCaret cs)) <> "s infinite backwards; }"
  , "  @keyframes blink { 0%, 50% { opacity: 1; } 50.01%, 100% { opacity: 0; } }"
  , "  @media (prefers-reduced-motion: reduce) {"
  , "    .fx { animation: none; opacity: 1; }"
  , "    .cursor { animation: none; opacity: 1; }"
  , "  }"
  , "</style>"
  ]

-- | Which step of the reveal cascade each part of the card animates on.
--
-- These used to be written out as literals, which meant removing a statistics
-- row left the caret, the language bar and the footer animating in the wrong
-- order with a visible gap in the middle. Deriving them from the row and
-- language counts keeps the sequence contiguous whatever the card shows.
data Cascade = Cascade
  { csCaret  :: Int
  , csBar    :: Int
  , csLegend :: Int
  }

-- | @cascade rowCount languageCount@. The bar holds one segment per language
-- plus an optional remainder, hence the extra step before the legend.
cascade :: Int -> Int -> Cascade
cascade nRows nLangs = Cascade
  { csCaret  = 2 + nRows
  , csBar    = 3 + nRows
  , csLegend = 4 + nRows + nLangs
  }

-- | Seconds before element @n@ of the cascade appears.
delayFor :: Int -> Double
delayFor n = 0.15 + 0.09 * fromIntegral n

-- | The class and delay attributes that place an element in the reveal
-- cascade. Style classes are passed in rather than written separately: a
-- second @class@ attribute is silently dropped by the XML parser, which would
-- leave the element either unstyled or unanimated.
anim :: Int -> [Text] -> Text
anim = animAt . delayFor

-- | As 'anim', but for elements timed in seconds rather than on the step grid.
animAt :: Double -> [Text] -> Text
animAt delay classes =
  " class=\"" <> T.unwords ("fx" : classes) <> "\" style=\"animation-delay:"
    <> secs delay <> "s\""

fx :: Int -> Text
fx n = anim n []

-- | Window frame: rounded card, traffic lights, title.
chrome :: Theme -> Config -> Text
chrome th cfg = T.unlines
  [ "<rect x=\"0.5\" y=\"0.5\" width=\"" <> tshow (cardW - 1) <> "\" height=\"" <> tshow (cardH - 1)
      <> "\" rx=\"10\" fill=\"" <> thBackground th <> "\" stroke=\"" <> thBorder th <> "\"/>"
  , "<line x1=\"1\" y1=\"" <> tshow titleH <> "\" x2=\"" <> tshow (cardW - 1) <> "\" y2=\""
      <> tshow titleH <> "\" stroke=\"" <> thBorder th <> "\"/>"
  , "<circle cx=\"24\" cy=\"20\" r=\"6\" fill=\"#ff5f57\"/>"
  , "<circle cx=\"44\" cy=\"20\" r=\"6\" fill=\"#febc2e\"/>"
  , "<circle cx=\"64\" cy=\"20\" r=\"6\" fill=\"#28c840\"/>"
  , "<text x=\"" <> tshow (cardW `div` 2) <> "\" y=\"21\" text-anchor=\"middle\" class=\"mut\">"
      <> escapeXml (cfgLogin cfg) <> " — zsh</text>"
  ]

-- | The left panel: the Haskell logo unless @assets\/ascii.txt@ supplies text
-- art to use instead.
panel :: Theme -> Text -> [Text]
panel th ascii
  | T.null (T.strip ascii) = logoPanel th
  | otherwise              = asciiPanel ascii

-- | Text art, one @\<text\>@ per row, revealed as a quick scan down the panel.
--
-- The rows are timed in seconds rather than on the cascade's step grid: a
-- portrait is 50-odd rows, and one step each would take it nearly five seconds
-- to draw, long after the statistics beside it had settled. The whole scan is
-- budgeted to finish first instead.
asciiPanel :: Text -> [Text]
asciiPanel ascii =
  [ "<text x=\"" <> tshow asciiX <> "\" y=\"" <> tshow (top + i * asciiLead)
      <> "\" xml:space=\"preserve\"" <> animAt (scanAt i) ["ascii"] <> ">"
      <> escapeXml line <> "</text>"
  | (i, line) <- zip [0 ..] rows
  ]
  where
    rows      = T.lines ascii
    -- Top-aligned with the prompt line. Both are centred on their own y, so
    -- matching their top edges means offsetting by half of each type size.
    top       = headY - promptSize `div` 2 + asciiSize `div` 2
    scanAt i  = delayFor 0 + scanSpan * fromIntegral i / fromIntegral (max 1 (length rows - 1))
    scanSpan  = 0.6 :: Double

-- | The Haskell logo as vector paths.
--
-- This started as ASCII art and could not be made to work: a monospace cell is
-- roughly 0.6 as wide as it is tall, so stepping one column per row draws a 60°
-- line out of 45° glyphs, and the strokes come out visibly dashed at any
-- leading. The panel is the one part of the card that is pure decoration, so it
-- is worth drawing properly. The outer group carries the animation because a
-- CSS transform would otherwise override the transform attribute placing it.
logoPanel :: Theme -> [Text]
logoPanel th =
  [ "<g" <> fx 0 <> ">"
  , "<svg x=\"" <> tshow logoX <> "\" y=\"" <> tshow logoY <> "\" width=\"" <> tshow logoW
      <> "\" height=\"" <> tshow logoH <> "\" viewBox=\"0 0 481 340\" fill=\""
      <> thAscii th <> "\">"
  ]
  <> map shape shapes
  <> [ "</svg>", "</g>" ]
  where
    -- A nested viewBox scales the artwork exactly; a scale() factor would have
    -- to be rounded to a printable number first.
    logoH = round (fromIntegral logoW * 340 / 481 :: Double) :: Int
    shape (op, pts) =
      "  <polygon points=\"" <> pts <> "\" fill-opacity=\"" <> op <> "\"/>"
    -- The mark is four polygons: the chevron, the lambda, and the two bars.
    shapes =
      [ ("1", "0,340 113.333,170 0,0 85,0 198.333,170 85,340")
      , ("1", "113.333,340 226.666,170 113.333,0 198.333,0 425,340 340,340 \
              \269.166,233.333 198.333,340")
      , ("0.5", "330.836,206.667 293.336,150 481,150 481,206.667")
      , ("0.5", "387.503,121.667 350.003,65 481,65 481,121.667")
      ]

-- | The @user\@host@ line, the build date sitting flush with the right edge of
-- the content column, and the rule under both.
promptLine :: Theme -> Config -> Stats -> [Text]
promptLine th cfg st =
  [ "<text x=\"" <> tshow keyX <> "\" y=\"" <> tshow headY <> "\"" <> anim 0 ["prompt"] <> ">"
      <> escapeXml (cfgTitle cfg) <> "</text>"
  , "<text x=\"" <> tshow contentRight <> "\" y=\"" <> tshow headY
      <> "\" text-anchor=\"end\"" <> anim 0 ["mut"] <> ">" <> buildDate <> "</text>"
  , "<line x1=\"" <> tshow keyX <> "\" y1=\"" <> tshow ruleY <> "\" x2=\"" <> tshow contentRight
      <> "\" y2=\"" <> tshow ruleY <> "\" stroke=\"" <> thBorder th <> "\"" <> fx 1 <> "/>"
  ]
  where
    buildDate = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (stGeneratedAt st))

row :: Theme -> Int -> (Text, [Span]) -> [Text]
row _ i (label, spans) =
  [ "<text x=\"" <> tshow keyX <> "\" y=\"" <> tshow y <> "\"" <> anim d ["key"] <> ">"
      <> escapeXml label <> ":</text>"
  , "<text x=\"" <> tshow valX <> "\" y=\"" <> tshow y <> "\"" <> fx d <> ">"
      <> T.concat (map spanTag spans) <> "</text>"
  ]
  where
    y = rowY + i * rowLead
    d = i + 2

spanTag :: Span -> Text
spanTag (Plain  t) = "<tspan class=\"val\">" <> escapeXml t <> "</tspan>"
spanTag (Accent t) = "<tspan class=\"acc\">" <> escapeXml t <> "</tspan>"
spanTag (Muted  t) = "<tspan class=\"mut\">" <> escapeXml t <> "</tspan>"
spanTag (Added  t) = "<tspan class=\"add\">" <> escapeXml t <> "</tspan>"
spanTag (Deleted t) = "<tspan class=\"del\">" <> escapeXml t <> "</tspan>"

-- | GitHub-style stacked language bar. The trailing segment covers everything
-- outside the top N, so the widths add up to the real distribution.
languageBar :: Theme -> Stats -> Cascade -> [Text]
languageBar th st cs
  | total <= 0 = []
  | otherwise  = clip : segments
  where
    total    = max 1 (stLangTotal st)
    langs    = stLangs st
    widths   = [ (lsColor l, fromIntegral (lsBytes l) / fromIntegral total * fromIntegral barW)
               | l <- langs ]
    used     = sum (map snd widths)
    rest     = fromIntegral barW - used
    pieces   = widths <> [(thBorder th, rest) | rest > 1]
    offsets  = scanl (+) (fromIntegral barX) (map snd pieces)

    clip = "<clipPath id=\"barclip\"><rect x=\"" <> tshow barX <> "\" y=\"" <> tshow barY
             <> "\" width=\"" <> tshow barW <> "\" height=\"" <> tshow barH <> "\" rx=\""
             <> tshow (barH `div` 2) <> "\"/></clipPath>"

    segments =
      [ "<rect x=\"" <> num x <> "\" y=\"" <> tshow barY <> "\" width=\"" <> num (w + 0.6)
          <> "\" height=\"" <> tshow barH <> "\" fill=\"" <> escapeXml colr
          <> "\" clip-path=\"url(#barclip)\"" <> fx (csBar cs + i) <> "/>"
      | (i, (x, (colr, w))) <- zip [0 ..] (zip offsets pieces)
      ]

-- | A waiting shell prompt on the line after the last statistics row.
caret :: Cascade -> Int -> [Text]
caret cs nRows =
  [ "<text x=\"" <> tshow keyX <> "\" y=\"" <> tshow y <> "\""
      <> anim (csCaret cs) ["prompt"] <> ">$</text>"
  , "<rect x=\"" <> tshow (keyX + 14) <> "\" y=\"" <> tshow (y - 9)
      <> "\" width=\"9\" height=\"17\" class=\"cursor\"/>"
  ]
  where
    y = rowY + nRows * rowLead

legend :: Stats -> Cascade -> [Text]
legend st cs = concat
  [ [ "<circle cx=\"" <> tshow (x + 5) <> "\" cy=\"" <> tshow y <> "\" r=\"5\" fill=\""
        <> escapeXml (lsColor l) <> "\"" <> fx d <> "/>"
    , "<text x=\"" <> tshow (x + 18) <> "\" y=\"" <> tshow (y + 1) <> "\""
        <> anim d ["val"] <> ">" <> escapeXml (lsName l) <> " <tspan class=\"mut\">"
        <> percent l <> "</tspan></text>"
    ]
  | (i, l) <- zip [0 ..] (stLangs st)
  , let x = barX + (i `mod` 3) * legendColW
        y = legendY + (i `div` 3) * legendLead
        d = csLegend cs + i
  ]
  where
    total = max 1 (stLangTotal st)
    percent l = num (fromIntegral (lsBytes l) / fromIntegral total * 100) <> "%"

-- | Two decimal places, for animation delays.
secs :: Double -> Text
secs x = T.pack (show (fromIntegral (round (x * 100) :: Int) / 100 :: Double))

-- | One decimal place, without the noise 'show' puts on a 'Double'.
num :: Double -> Text
num x = T.pack (showFixed (fromIntegral (round (x * 10) :: Int) / 10))
  where
    showFixed v =
      let s = show (v :: Double)
      in if ".0" `isSuffix` s then take (length s - 2) s else s
    isSuffix suf s = suf == drop (length s - length suf) s
