module Main where

import Types
import Svgs (favicon, logo, hamburger)
import Style ( styleSheet, styleSheetFont, fontLogo, fontPrimary )

import Lucid
import qualified Clay
import Clay.Render
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import qualified Data.List as List
import System.Directory ( listDirectory )
import System.FilePath ( takeExtension, takeFileName, (</>), (<.>) )
import qualified Network.URI.Encode as Uri

outputPath :: FilePath
outputPath = "dist"

main :: IO ()
main = do

  -- routes
  let routes = [minBound :: Route ..]
  -- TODO add not found 404.html

  -- load all resources
  svgs <- loadFilesFrom ("resources" </> "icons") loadFileAsHtml
  imgs <- loadFilesFrom ("resources" </> "images") loadFileAsBase64
  fonts <- loadFilesFrom ("resources" </> "fonts") loadFileAsBase64

  -- setup the model
  let model = Model
        { mySocialLinks =
          [ ("https://github.com/tamaw", lookupResource "github.svg" svgs)
          , ("https://dev.to/tamaw", lookupResource "devdotto.svg" svgs)
          , ("https://www.linkedin.com/in/tama-waddell/", lookupResource "linkedin.svg" svgs)
          , ("https://twitter.com/twaddell_", lookupResource "twitter.svg" svgs)
          , ("https://exercism.org/profiles/tamaw", lookupResource "exercism.svg" svgs)
          , ("https://stackoverflow.com/users/4778435/tama", lookupResource "stackoverflow.svg" svgs)
          ]
          , myProfilePic = lookupResource "profile.jpg" imgs
          , myRoutes = routes
          , myLogoFont = lookupResource "exo2-bold-webfont.woff2" fonts
          , myPrimaryFont = lookupResource "jost-400-book-webfont.woff2" fonts
        }

  -- write out html
  forM_ routes (\r -> renderToFile (outputPath </> routeToFileName r) $ masterHtml r model)

  where
    loadFilesFrom fp l = listDirectory fp >>= mapM (l . (fp </>))
    loadFileAsHtml = loadFile toHtmlRaw
    loadFileAsBase64 f = loadFile (\b -> getDataHeader f <> "base64," <> BS.encodeBase64 b) f
    lookupResource n r = fromJust $ List.lookup n r

routeToFileName :: Route -> FileName
routeToFileName r =  map toLower (show r) <.> "html"

loadFile :: (BS.ByteString -> a) -> FilePath -> IO (FileName, a)
loadFile convert filePath = do
  content <- BS.readFile filePath
  return (takeFileName filePath, convert content)

getDataHeader :: FilePath -> Text
getDataHeader f = case takeExtension f of
  ".jpg" -> "data:image/jpeg;"
  ".jpeg" -> "data:image/jpeg;"
  ".png" -> "data:image/png;"
  ".woff2" -> "data:file/octet-stream;"
  ".svg" -> "data:image/svg+xml;" -- todo user with favicon
  _ -> error $ "Unknown image format for: " <> f

masterHtml :: Route -> Model -> Html ()
masterHtml r m = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ headHtml m
    body_ $ do
      header_ $ do
        a_ [href_ "#", class_ "logo"] logo
        navHtml m
      main_ $ case r of
          Index -> p_ "content"
          Blog -> do
            p_ "contenta"
            div_ [style_ "width:40px"] hamburger
          _ -> return ()

headHtml :: Model -> Html ()
headHtml m = do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ "tama:w"
  -- meta_ [name_ "description", content_ "replace me" ]
  link_ [rel_ "icon", href_ ("data:image/svg+xml," <> urlEncodedFavIco favicon)]
  style_ [type_ "text/css"] $ toStrict (Clay.renderWith prettyConfig [] styleSheet)
  style_ [type_ "text/css"] $ toStrict (Clay.renderWith Clay.compact [] $ styleSheetFont fontLogo (myLogoFont m))
  style_ [type_ "text/css"] $ toStrict (Clay.renderWith prettyConfig [] $ styleSheetFont fontPrimary (myPrimaryFont m))

  where
    prettyConfig = pretty { Clay.Render.banner = False }
    urlEncodedFavIco i = Uri.encodeText . toStrict $ renderText i

navHtml :: Model -> Html ()
navHtml m = do
  input_ [class_ "menu-btn hidden", type_ "checkbox", id_ "menu-btn"]
  label_ [class_ "menu-icon", for_ "menu-btn"] $ do
    span_ [class_ "hamburger"] hamburger
  ul_ [class_ "menu"] $ forM_ (myRoutes m) renderNavItem
  where
    renderNavItem :: Route -> Html ()
    renderNavItem r = li_ $ a_ [href_ . T.pack $ routeToFileName r] (toHtml $ show r)
