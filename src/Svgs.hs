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
import Control.Monad (forM_)

feDropShadow_ :: Monad m => [Attribute] -> SvgT m ()
feDropShadow_ = with $ makeXmlElementNoEnd "feDropShadow"

svg2_ :: Term [Attribute] (s -> t) => s -> t
svg2_ = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg" ]

renderCss :: Css -> String
renderCss css = unpack . toStrict $ Clay.renderWith Clay.compact [] css

favicon :: Svg ()
favicon =
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

hamburger :: Svg ()
hamburger =
  with (svg2_ contentsSvg) [viewBox_ "0 0 16 16"]
  where
    contentsSvg = do
      rect_ [y_ "1", width_ "16", height_ "2"]
      rect_ [y_ "7", width_ "16", height_ "2"]
      rect_ [y_ "13", width_ "16", height_ "2"]

logo :: Svg ()
logo =
  with (svg2_ contentsSvg) [width_ "150px", height_ "30px"]
  where
    contentsSvg = do
      style_ . fromString $ renderCss styleSvg
      defs_ defsSvg
      text_ [y_ "25", x_ "0"] "tama"
      text_ [y_ "24", x_ "80"] ":"
      text_ [y_ "25", x_ "89"] "w"

    defsSvg =
      filter_ [id_ "text-glow"] $ do
      feGaussianBlur_ [in_ "SourceGraphic", result_ "blur", stdDeviation_ ".3"]
      feDropShadow_ [in_ "blur", result_ "shadow", flood_color_ "var(--text-colour)", dx_ "0", dy_ "1", flood_opacity_ ".4" ]
      feComposite_ [in_ "SourceGraphic", in2_ "shadow", operator_ "over"]

    styleSvg = do
      "text" ? do
        fontFamily [fontLogo] []
        fontSize (px 32)
        fontWeight bold
        "filter" -: "url(#text-glow)"
        "fill" -: "var(--text-colour)"
      "text" # nthOfType "2" ? do
        "fill" -: "var(--accent-colour)"

banner :: Svg ()
banner =
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
        term "feDiffuseLighting" [result_ "backlight", lighting_color_ "var(--text-colour)", surfaceScale_ "1", diffuseConstant_ "1", kernelUnitLength_ "1"] $
          fePointLight_ [x_ "65", y_ "100", z_ "145"]
        feComposite_ [in_ "SourceGraphic", in2_ "backlight", operator_ "arithmetic", k1_ "1", k2_ "0", k3_ "0", k4_ "0"]

      filter_ [id_ "bottom-glow"] $
        feDropShadow_ [in_ "SourceGraphic", flood_color_ "var(--text-colour)", dx_ "1", dy_ "2", flood_opacity_ ".4"]
      filter_ [id_ "bottom-glow-small"] $
        feDropShadow_ [in_ "SourceGraphic", flood_color_ "var(--text-colour)", dx_ "1", dy_ "1", flood_opacity_ ".2"]
      filter_ [id_ "back-glow"] $
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

      let plots = [(80, -10, 245, -120, -135, 95)
                  ,(68, -6, 119, -65, -60, 95)
                  ,(60, -8, 45, -110, 57, 95)
                  ,(50, -4, -65, -30, 91, 95)
                  ,(44, -8, -105, -50, 147, 95)
                  ,(34, -4, -87, -25, 108, 50)
                  ,(28, -4, -120, -25, 141, 50)
                  ,(22, -7, -157, -25, 175, 50)
                  ,(12, -2, -197, -25, 220, 50)
                  ,(8, -2, -240, -25, 263, 50)
                  ,(4, -4, -277, -25, 298, 50)
                  ]

      forM_ plots drawBar

      where
        barHeight = 120.0 :: Double
        lineGap = 0.13 -- attempts to cover the middle line

        drawBar (x, w, l0, l1, l2, h) = do
          drawTopBar_ x 0 w barHeight
          drawBottomBar_ x barHeight h l0 l1 l2

        drawTopBar_ x y w h =
            path_ [d_ [iii|M #{x} #{y}
                           v #{h + lineGap}
                           h #{w} 0
                           v 0 -#{h}
                           z
                      |]
                  ]
        drawBottomBar_ x y h l0 l1 l2 =
            path_ [d_ [iii|M #{x} #{y}
                           l #{l0} #{h}
                           l #{l1} 0
                           l #{l2} -#{h}
                      |]
                 ]


