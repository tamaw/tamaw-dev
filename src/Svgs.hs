{-# LANGUAGE FlexibleContexts #-}

module Svgs where

import Style
import Lucid.Base
import Lucid.Svg
import Clay
import Data.Text.Lazy (toStrict)
import Data.Text (unpack)
import Data.String (fromString)
import Data.String.Interpolate (iii)

feDropShadow_ :: Monad m => [Attribute] -> SvgT m ()
feDropShadow_ = with $ makeXmlElementNoEnd "feDropShadow"

svg2_ :: Term [Attribute] (s -> t) => s -> t
svg2_ = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg" ]

renderCss :: Css -> String
renderCss css = unpack . toStrict $ Clay.renderWith Clay.compact [] css

favicon :: Svg ()
favicon = do
  with (svg2_ contentsSvg) [class_ "favicon", viewBox_ "0 0 16 16"]
  where
    textHeight = "16"
    symbolHeight = "15.5"

    contentsSvg = do
      style_ . fromString $ renderCss styleSvg
      text_ [y_ textHeight, x_ "1"] "t"
      text_ [y_ symbolHeight, x_ "7"] ":"

    styleSvg = do
      "text" ? do
        fontFamily [fontHeading] [sansSerif]
        fontSize (px 20)
        "fill" -: accentColour

logo :: Svg ()
logo = do
  with (svg2_ contentsSvg) [class_ "logo"]
  where
    contentsSvg = do
      style_ . fromString $ renderCss styleSvg
      defs_ defsSvg
      text_ [filter_ "url(#text-glow)", y_ "25", x_ "0"] "tama"
      text_ [filter_ "url(#text-glow)", y_ "24", x_ "80", class_ "colon"] ":"
      text_ [filter_ "url(#text-glow)", y_ "25", x_ "89"] "w"

    defsSvg = do
      filter_ [id_ "text-glow"] $ do
        feGaussianBlur_ [in_ "SourceGraphic", result_ "blur", stdDeviation_ ".3"]
        feDropShadow_ [in_ "blur", result_ "shadow", flood_color_ "white", dx_ "0", dy_ "1", flood_opacity_ ".4" ]
        feComposite_ [in_ "SourceGraphic", in2_ "shadow", operator_ "over"]

    styleSvg = do
      ".logo" ? do
        fontFamily [fontLogo] []
        fontSize (px 32)
        fontWeight bold
        "#text-glow" ? "feDropShadow" ? do
          "flood-color" -: "var(--text-colour)"
        "fill" -: "var(--text-colour)"
        ".colon" ? do
          "fill" -: "var(--accent-colour)"

banner :: Svg ()
banner = do
  with (svg2_ contentsSvg) [width_ "3000px", class_ "banner", viewBox_ "0 1 800 215"]
  where
    contentsSvg = do
      style_ . fromString $ renderCss styleSvg
      defs_ defsSvg
      g_ drawSvg

    defsSvg = do
      radialGradient_ [id_ "gradient-top", cx_ "0.0", cy_ "0.9", r_ "1.4", fx_ "-0.19", fy_ "1", spreadMethod_ "pad" ] $ do
        stop_ [offset_ "0%", stop_color_ "var(--shade-colour)"]
        stop_ [offset_ "60%", stop_color_ "var(--primary-colour)"]
      radialGradient_ [id_ "gradient-bottom", cx_ "0.0", cy_ "0.6", r_ "1.3", fx_ "-0.19", fy_ "1", spreadMethod_ "pad" ] $ do
        stop_ [offset_ "0%", stop_color_ "var(--shade-colour)"]
        stop_ [offset_ "65%", stop_color_ "var(--primary-colour)"]

      filter_ [id_ "point-light"] $ do
        feGaussianBlur_ [in_ "SourceGraphic", stdDeviation_ "10"]
        term "feDiffuseLighting" [result_ "backlight", lighting_color_ "var(--text-colour)", surfaceScale_ "1", diffuseConstant_ "1", kernelUnitLength_ "1"] $ do
          fePointLight_ [x_ "65", y_ "100", z_ "145"]
        feComposite_ [in_ "SourceGraphic", in2_ "backlight", operator_ "arithmetic", k1_ "1", k2_ "0", k3_ "0", k4_ "0"]

      filter_ [id_ "bottom-glow"] $ do
        feDropShadow_ [in_ "SourceGraphic", flood_color_ "var(--text-colour)", dx_ "1", dy_ "2", flood_opacity_ ".4"]
      filter_ [id_ "bottom-glow-small"] $ do
        feDropShadow_ [in_ "SourceGraphic", flood_color_ "var(--text-colour)", dx_ "1", dy_ "1", flood_opacity_ ".2"]
      filter_ [id_ "back-glow"] $ do
        feDropShadow_ [in_ "SourceGraphic", flood_color_ "var(--text-colour)", dx_ "1", dy_ "-1", flood_opacity_ ".01"]

    styleSvg = do
      "path" ? do
        "fill" -: "var(--text-colour)"

      "path" # firstOfType ? do
        "stroke" -: "var(--opposite-text-colour)"
        "stroke-width" -: "0.2"

      "path" # nthOfType "even" ? do
        "stroke" -: "var(--opposite-text-colour)"
        "stroke-width" -: "0.2"
        "filter" -: "url(#back-glow)"

      "path" # nthOfType "odd" ? do
        "filter" -: "url(#point-light) url(#bottom-glow)"

      "path" # nthOfType "4" <> "path" # nthOfType "5" ? do
        "stroke-width" -: "0"
        "fill" -: "var(--accent-colour)"
        "filter" -: "url(#bottom-glow)"

    drawSvg = do
      rect_ [id_ "top-bg", x_ "0", y_ "0", rx_ "0", ry_ "0", width_ "400", height_ "120", fill_ "url(#gradient-top)"]
      rect_ [id_ "bottom-bg", x_ "0", y_ "120", rx_ "0", ry_ "0", width_ "400", height_ "95", fill_ "url(#gradient-bottom)"]

      path_ [d_ "M 0 120 h 400"]

      drawTopBar 80 (-10)
      drawBottomBar 80 95 245 (-120) (-135)

      drawTopBar 68 (-6)
      drawBottomBar 68 95 119 (-65) (-60)

      drawTopBar 60 (-8)
      drawBottomBar 60 95 45 (-110) 57

      drawTopBar 50 (-4)
      drawBottomBar 50 95 (-65) (-30) 91

      drawTopBar 44 (-8)
      drawBottomBar 44 95 (-105) (-50) 147

      drawTopBar 34 (-4)
      drawBottomBar 34 50 (-87) (-25) 108

      drawTopBar 28 (-4)
      drawBottomBar 28 50 (-120) (-25) 141

      drawTopBar 22 (-7)
      drawBottomBar 22 50 (-157) (-25) 175

      drawTopBar 12 (-2)
      drawBottomBar 12 50 (-197) (-25) 220

      drawTopBar 8 (-2)
      drawBottomBar 8 50 (-240) (-25) 263

      drawTopBar 4 (-4)
      drawBottomBar 4 50 (-277) (-25) 298

      where
        lineGap = 0.13 -- attempts to cover the middle line
        barHeight = 120.0 :: Double

        -- drawTopBar :: Double -> Double -> SvgT m ()
        drawTopBar m1 h1 = do
            path_ [d_ [iii|M #{m1} 0
                           v #{barHeight + lineGap}
                           h #{h1} 0
                           v 0 -#{barHeight}
                           z
                      |]
                  ]

        drawBottomBar m1 h1 l1 l2 l3 = do
            path_ [d_ [iii|M #{m1} #{barHeight}
                           l #{l1} #{h1}
                           l #{l2} 0
                           l #{l3} -#{h1}
                      |]
                  ]





