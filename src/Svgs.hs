{-# LANGUAGE FlexibleContexts #-}

module Svgs where

import Lucid.Base
import Lucid.Svg
import Clay
import Data.Text.Lazy (toStrict)
import Data.Text (unpack)
import Data.String (fromString)

svg2_ :: Term [Attribute] (s -> t) => s -> t
svg2_ = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg" ]

renderCss :: Css -> String
renderCss css = unpack . toStrict $ Clay.renderWith Clay.compact [] css

favicon :: Svg ()
favicon = do
  with (svg2_ contentsSvg) [class_ "favicon", viewBox_ "0 0 16 16"]

  where
    contentsSvg = do
      style_ . fromString $ renderCss styleSvg
      text_ [y_ "16", x_ "3"] "t"
      text_ [y_ "15.5", x_ "10"] ":"

    styleSvg = do
      "text" ? do
        fontFamily ["exo"] [sansSerif]
        fontSize (px 20)
        "fill" -: "#FE5D26"

