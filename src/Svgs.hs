{-# LANGUAGE FlexibleContexts #-}

module Svgs where

import Style
import Lucid.Base
import Lucid.Svg
import Clay
import Data.Text.Lazy (toStrict)
import Data.Text (unpack)
import Data.String (fromString)

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
      term "filter" [id_ "text-glow"] $ do
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
  with (svg2_ contentsSvg) [class_ "favicon", viewBox_ "0 0 16 16"]
  where
    contentsSvg = do
      style_ . fromString $ renderCss styleSvg
    styleSvg = do
      "light" ? do
        "path" -: "var(--primary-colour)"




