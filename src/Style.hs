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
fontPrimary = "Jost"
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

styleSheetFont :: Text -> Base64 -> Css
styleSheetFont fontName fontData = do
  fontFace $ do
    fontFamily [fontName] []
    fontFaceSrc [FontFaceSrcUrl fontData (Just WOFF2)]

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

  -- query M.screen [Feature "prefers-color-scheme" $ Just "dark"] $ do
  --   ":root" ? do
  --     "--text-colour" -: "white"
  --     "--opposite-text-colour" -: "black"
  --     "--primary-colour" -: blackPrimaryColour
  --     "--secondary-colour" -: blackSecondaryColour
  --     "--shade-colour" -: darkShadeColour

  ":root.dark" ? do
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
    width (pct 100)
    position fixed
    backgroundColor (other "var(--opposite-text-colour)")
    boxShadow $ fromList [bsColor (other "var(--text-colour)") $ shadowWithBlur (px 0) (px 1) (px 4)]

    li ? a ? do
      display block
      borderRight (px 1) solid (other "var(--secondary-colour)")
      sym padding (px 15)
      textDecoration none
      color $ other "var(--text-colour)"
      hover & do
        color $ other "var(--accent-colour)"
        "transition" -: "color 0.3s ease"

    ".logo" ? do
      display inlineBlock
      float floatLeft
      sym2 margin (px 3) (px 5)

    ".menu" ? do
      clear both
      overflow hidden
      maxHeight (px 0)
      userSelect none
      height none
      transition "max-height" (sec 0.2) easeOut (ms 0)

    ".menu-btn" # checked |~ ".menu" ? do
      height auto
      maxHeight (px 1000)

    ".menu-icon" ? do
      cursor pointer
      display inlineBlock
      float floatRight
      sym2 margin (px 10) (px 15)
      ".hamburger" ? do
        display block
        height (px 2)
        width (px 24)

  query M.screen [M.minWidth (em 48)] $ do
    header ? do
      ul ? do
        listStyleType none
      li ? do
        float floatLeft
        a ? do
         "border-right" -: "none"
         sym2 padding (px 10) (px 20)
      ".menu" ? do
        clear none
        float floatRight
        maxHeight none
      ".menu-icon" ? display none

  "main" ? do
    paddingTop (px 50)

