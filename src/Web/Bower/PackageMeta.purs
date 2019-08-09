-- | Decoders for various types representing `bower.json` contents.
-- |
-- | `PackageMeta` represents a valid `bower.json` file.
-- |
-- | The spec: https://github.com/bower/spec/blob/master/json.md
module Web.Bower.PackageMeta where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Core (Json, caseJsonString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

newtype VersionRange = VersionRange String

derive instance newtypeVersionRange :: Newtype VersionRange _
derive instance eqVersionRange :: Eq VersionRange
derive instance genericVersionRange :: Generic VersionRange _
instance showVersionRange :: Show VersionRange where
  show = genericShow
derive newtype instance decodeJsonVersionRange :: DecodeJson VersionRange

newtype Dependencies =
  Dependencies (Array { packageName :: String
                      , versionRange :: VersionRange
                      })

derive instance newtypeDependencies :: Newtype Dependencies _
derive instance eqDependencies :: Eq Dependencies
derive instance genericDependencies :: Generic Dependencies _
instance showDependencies :: Show Dependencies where
  show = genericShow

instance decodeJsonDependencies :: DecodeJson Dependencies where
  decodeJson json =
    Dependencies <$> fromObject <$> decodeJson json
    where
      fromObject obj =
        Object.toUnfoldable obj <#>
        \(Tuple packageName versionRange) -> { packageName, versionRange }

newtype Version = Version String

derive instance newtypeVersion :: Newtype Version _
derive instance eqVersion :: Eq Version
derive instance genericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
derive newtype instance decodeJsonVersion :: DecodeJson Version

newtype Resolutions =
  Resolutions (Array { packageName :: String
                     , version :: Version
                     })

derive instance newtypeResolution :: Newtype Resolutions _
derive instance eqResolutions :: Eq Resolutions
derive instance genericResolutions :: Generic Resolutions _
instance showResolutions :: Show Resolutions where
  show = genericShow

instance decodeJsonResolution :: DecodeJson Resolutions where
  decodeJson json =
    Resolutions <$> fromObject <$> decodeJson json
    where
      fromObject obj =
        Object.toUnfoldable obj <#>
        \(Tuple packageName version) -> { packageName, version }

newtype Repository =
  Repository
  { url :: String
  , type :: String
  }

derive instance newtypeRepository :: Newtype Repository _
derive instance eqRepository :: Eq Repository
derive instance genericRepository :: Generic Repository _
instance showRepository :: Show Repository where
  show = genericShow

instance decodeJsonRepository :: DecodeJson Repository where
  decodeJson json = do
    x     <- decodeJson json
    url   <- x .: "url"
    type_ <- x .: "type"
    pure $ Repository { type: type_, url }

data ModuleType
  = Globals
  | AMD
  | Node
  | ES6
  | YUI

derive instance eqModuleType :: Eq ModuleType
derive instance genericModuleType :: Generic ModuleType _
instance showModuleType :: Show ModuleType where
  show = genericShow

instance decodeJsonModuleType :: DecodeJson ModuleType where
  decodeJson json =
    caseJsonString (err "Not a string")
    (\str -> case str of
        "globals" -> Right Globals
        "amd"     -> Right AMD
        "node"    -> Right Node
        "es6"     -> Right ES6
        "yui"     -> Right YUI
        value     -> err value)
    json
    where
      err value = Left $ "Incorrect module format: " <> value

-- | If an author is specified as a `String`, e.g.:
-- |
-- | ```
-- | "author": "Author Name <author@gmail.com>",
-- | ```
-- |
-- | this string will not be parsed, and this entry will be decoded as
-- |
-- | ```
-- | Author { name: Just "Author Name <author@gmail.com>"
-- |        , email: Nothing
-- |        , homepage: Nothing }
-- | ```
newtype Author
  = Author
    { name     :: Maybe String
    , email    :: Maybe String
    , homepage :: Maybe String
    }

derive instance newtypeAuthor :: Newtype Author _
derive instance eqAuthor :: Eq Author
derive instance genericAuthor :: Generic Author _
instance showAuthor :: Show Author where
  show = genericShow

instance decodeJsonAuthor :: DecodeJson Author where
  decodeJson json =
    caseJsonString
    (do
        x        <- decodeJson json
        name     <- x .:? "name"
        email    <- x .:? "email"
        homepage <- x .:? "homepage"
        pure $ Author { name, email, homepage })
    (\str -> pure $ Author { name: Just str, email: Nothing, homepage: Nothing })
    json

newtype PackageMeta = PackageMeta
  { name            :: String
  , description     :: Maybe String
  , main            :: Maybe (Array String)
  , moduleType      :: Maybe (Array ModuleType)
  , license         :: Maybe (Array String)
  , ignore          :: Maybe (Array String)
  , keywords        :: Maybe (Array String)
  , authors         :: Maybe (Array Author)
  , homepage        :: Maybe String
  , repository      :: Maybe Repository
  , dependencies    :: Dependencies
  , devDependencies :: Dependencies
  , resolutions     :: Maybe Resolutions
  , private         :: Maybe Boolean
  }

derive instance newtypePackageMeta :: Newtype PackageMeta _
derive instance eqPackageMeta :: Eq PackageMeta
derive instance genericPackageMeta :: Generic PackageMeta _
instance showPackageMeta :: Show PackageMeta where
  show = genericShow

instance decodeJsonPackageMeta :: DecodeJson PackageMeta where
  decodeJson json = do

    x               <- decodeJson json
    name            <-               x .:  "name"
    description     <-               x .:? "description"
    main            <- maybeMany =<< x .:? "main"
    moduleType      <- maybeMany =<< x .:? "moduleType"
    license         <- maybeMany =<< x .:? "license"
    ignore          <-               x .:? "ignore"
    keywords        <-               x .:? "keywords"
    dependencies    <-               x .:  "dependencies"
    devDependencies <-               x .:  "devDependencies"
    resolutions     <-               x .:? "resolutions"
    private         <-               x .:? "private"
    repository      <-               x .:? "repository"
    authors         <-               x .:? "authors"
    homepage        <-               x .:? "homepage"

    pure $ PackageMeta { name
                       , description
                       , main
                       , moduleType
                       , license
                       , ignore
                       , keywords
                       , resolutions
                       , private
                       , dependencies
                       , devDependencies
                       , repository
                       , authors
                       , homepage
                       }

-- | Decode a *recommended* field that may contain a single value
-- | or an array of values.
maybeMany
  :: forall a
  .  DecodeJson a
  => Maybe Json
  -> Either String (Maybe (Array a))
maybeMany = \x -> case x of
                Just value -> Just <$> multiple value
                Nothing -> Right Nothing

-- | Decode a single value or an array of values.
multiple
  :: forall a
  .  DecodeJson a
  => Json
  -> Either String (Array a)
multiple json =
  (pure <$> decodeJson json) <|> decodeJson json
