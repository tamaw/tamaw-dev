module Style where

import Clay
import Clay.Stylesheet (Feature (..))
import qualified Clay.Media as M
import Data.Text (Text)
import qualified Data.Text as T
import Types (Base64)
import Prelude hiding (not, (**))
import Data.List.NonEmpty ( fromList )

fontLogo, fontHeading, fontPrimary, fontSecondary :: Text
fontLogo = "Exo2"
fontHeading = "Helvetica"
fontPrimary = "Tahoma"
fontSecondary = "Segoe"

blackPrimaryColour, blackSecondaryColour :: Text
blackPrimaryColour = "#090A0B"
blackSecondaryColour = "#484748"
whitePrimaryColour, whiteSecondaryColour :: Text
whitePrimaryColour = "#FDF7FA"
whiteSecondaryColour = "#D6D6D6"
lightShadeColour, darkShadeColour, accentColour :: Text
lightShadeColour = "#6A7682"
darkShadeColour = "#474F57"
accentColour = "#FE5D26"

styleSheetFonts :: Base64 -> Css
styleSheetFonts d =
  fontFace $ do
    fontFamily [fontLogo] []
    fontFaceSrc [FontFaceSrcUrl d (Just WOFF2)]

showText :: Show a => a -> Text
showText = T.pack . show

styleSheet :: Css
styleSheet = do
  styleDefaults
  styleCommons
  styleHeader

styleDefaults :: Css
styleDefaults = do
  ":root" ? do
    "color-scheme" -: "light dark"
    "--text-colour" -: "black"
    "--opposite-text-colour" -: "white"
    "--primary-colour" -: whitePrimaryColour
    "--secondary-colour" -: whiteSecondaryColour
    "--shade-colour" -: lightShadeColour
    "--accent-colour" -: accentColour

  query M.screen [Feature "prefers-color-scheme" $ Just "dark"] $ do
    ":root" ? do
      "--text-colour" -: "white"
      "--opposite-text-colour" -: "black"
      "--primary-colour" -: blackPrimaryColour
      "--secondary-colour" -: blackSecondaryColour
      "--shade-colour" -: darkShadeColour

  star <> star # before <> star # after ? do
    boxSizing inherit
    sym margin (px 0)
    sym padding (px 0)

  html ? do
    boxSizing borderBox
    lineHeight (em 1.5)
    "color" -: "var(--text-colour)"
  body ? do
    fontFamily [fontPrimary] [sansSerif]
    "background-color" -: "var(--primary-colour)"

styleCommons :: Css
styleCommons = do
  ".flex" ? display flex
  ".flex-row" ? flexDirection row
  ".flex-col" ? flexDirection column

  ".items-center" ? alignItems center
  ".justify-center" ? justifyContent center
  ".h-screen" ? height (vh 100)
  ".relative" ? position relative

  ".wrapper-container" ? do
    width auto
    margin auto auto auto auto
  ".hidden" ? do
    display none

styleHeader :: Css
styleHeader = do
  header ? do
    boxShadow $ fromList [bsColor (other "var(--text-colour)") $ shadowWithBlur (px 1) (px 1) (px 4)]
    width (pct 100)
    backgroundColor white

    ul ? do
      overflow hidden


    li ? a ? do
      display block
      sym padding (px 15)
      borderRight (px 1) solid (other "var(--text-colour)")
      textDecoration none
      color $ other "var(--text-colour)"
      hover & do
        backgroundColor $ other "var(--primary-colour)"

    ".logo" ? do
      display inlineBlock
      float floatLeft
      sym2 padding (px 5) (px 10)

    ".menu" ? do
      clear both
      maxHeight (px 0)
      transition "max-height" (sec 0.2) easeOut (ms 0)

    ".menu-icon" ? do
      cursor pointer
      display inlineBlock
      float floatRight
      sym2 padding (px 10) (px 15)
      ".navicon" ? do
        display block
        height (px 2)
        width (px 24)

    ".menu-btn" # checked |~ ".menu" ?
      maxHeight (pct 100)

  query M.screen [M.minWidth (em 48)] $ do
    header ? do
      li ? do
        float floatLeft
        a ? sym2 padding (px 20) (px 30)
      ".menu" ? do
        clear none
        float floatRight
        maxHeight none
      ".menu-icon" ? display none
