module Types where
import Data.Text
import Lucid

type Uri = Text
type FileName = String
type Base64 = Text
type Svg = Html ()

data Route =
  Home
  | Blog
  | Speaking
  | Projects
  | Contact
  deriving (Bounded, Enum, Show)

data Model = Model
  { mySocialLinks :: [(Uri, Svg)]
  , myProfilePic :: Base64
  , myRoutes :: [Route]
  , myLogoFont :: Base64
  , myPrimaryFont :: Base64
  } deriving (Show)

