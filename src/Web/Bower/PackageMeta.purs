-- | Decoders for various types representing `bower.json` contents.
-- |
-- | The main type is `PackageMeta`.
-- |
-- | `BowerJson` represents a valid `bower.json` file (literally).
-- |
-- | The spec: https://github.com/bower/spec/blob/master/json.md
module Web.Bower.PackageMeta
       ( PackageMeta (..)
       , Dependencies (..)
       , Resolutions (..)
       , ModuleType (..)
       , Author (..)
       , BowerJson (..)
       , readPackageMeta
       , writePackageMeta
       )
where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Core (Json, caseJsonString, fromString, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | A human-friendly version of `BowerJson`.
-- |
-- | Decoding to and encoding from this type does not preserve fields with
-- | default values.
-- |
-- | For example, encoding a `PackageMeta` with `license: []` and
-- | `private: false` will result in a json object without these fields.
-- |
-- | Use `BowerJson` if these omissions are not desirable.
newtype PackageMeta = PackageMeta
  { name            :: String
  , description     :: Maybe String
  , main            :: Array String
  , moduleType      :: Array ModuleType
  , license         :: Array String
  , ignore          :: Array String
  , keywords        :: Array String
  , authors         :: Array Author
  , homepage        :: Maybe String
  , repository      :: Maybe { url :: String
                             , type :: String
                             }
  , dependencies    :: Dependencies
  , devDependencies :: Dependencies
  , resolutions     :: Resolutions
  , private         :: Boolean
  }

derive instance eqPackageMeta :: Eq PackageMeta
derive instance newtypePackageMeta :: Newtype PackageMeta _
derive instance genericPackageMeta :: Generic PackageMeta _
instance showPackageMeta :: Show PackageMeta where
  show = genericShow

instance decodeJsonPackageMeta :: DecodeJson PackageMeta where
  decodeJson = map readPackageMeta <<< decodeJson

instance encodeJsonPackageMeta :: EncodeJson PackageMeta where
  encodeJson = writePackageMeta >>> encodeJson

readPackageMeta :: BowerJson -> PackageMeta
readPackageMeta (BowerJson { name
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
                           }) =
  PackageMeta { name, description
              , main: fold main
              , moduleType: fold moduleType
              , license: fold license
              , ignore: fold ignore
              , keywords: fold keywords
              , resolutions: fold resolutions
              , private: fromMaybe false private
              , dependencies: fold dependencies
              , devDependencies: fold devDependencies
              , repository: repository
              , authors: fold authors
              , homepage: homepage
              }

writePackageMeta :: PackageMeta -> BowerJson
writePackageMeta (PackageMeta { name
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
                              }) =
  BowerJson { name, description
            , main: unfold main
            , moduleType: unfold moduleType
            , license: unfold license
            , ignore: unfold ignore
            , keywords: unfold keywords
            , resolutions: unfold resolutions
            , private: if private == false then Nothing else Just true
            , dependencies: unfold dependencies
            , devDependencies: unfold devDependencies
            , repository: repository
            , authors: unfold authors
            , homepage: homepage
            }
  where
    unfold :: forall a. Eq a => Monoid a => a -> Maybe a
    unfold x = if mempty == x then Nothing else Just x

-- | A `bower.json` file represented as it is.
-- | Decoding and encoding a `bower.json` to and from `BowerJson` does not change it
-- | (up to JSON reformatting).
-- |
-- | `BowerJson` type should only be used when the above property is needed.
-- |
-- | For other use cases, it is more convenient to use `PackageMeta` instead.
newtype BowerJson = BowerJson
  { name            :: String
  , description     :: Maybe String
  , main            :: Maybe (Array String)
  , moduleType      :: Maybe (Array ModuleType)
  , license         :: Maybe (Array String)
  , ignore          :: Maybe (Array String)
  , keywords        :: Maybe (Array String)
  , authors         :: Maybe (Array Author)
  , homepage        :: Maybe String
  , repository      :: Maybe { url :: String
                             , type :: String
                             }
  , dependencies    :: Maybe Dependencies
  , devDependencies :: Maybe Dependencies
  , resolutions     :: Maybe Resolutions
  , private         :: Maybe Boolean
  }

derive instance newtypeBowerJson :: Newtype BowerJson _
derive instance eqBowerJson :: Eq BowerJson
derive instance genericBowerJson :: Generic BowerJson _
instance showBowerJson :: Show BowerJson where
  show = genericShow

instance decodeJsonBowerJson :: DecodeJson BowerJson where
  decodeJson json = do

    x               <- decodeJson json
    name            <-               x .:  "name"
    description     <-               x .:? "description"
    main            <- maybeMany =<< x .:? "main"
    moduleType      <- maybeMany =<< x .:? "moduleType"
    license         <- maybeMany =<< x .:? "license"
    ignore          <-               x .:? "ignore"
    keywords        <-               x .:? "keywords"
    dependencies    <-               x .:? "dependencies"
    devDependencies <-               x .:? "devDependencies"
    resolutions     <-               x .:? "resolutions"
    private         <-               x .:? "private"
    repository      <-               x .:? "repository"
    authors         <-               x .:? "authors"
    homepage        <-               x .:? "homepage"

    pure $ BowerJson { name
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


instance encodeJsonBowerJson :: EncodeJson BowerJson where
  encodeJson (BowerJson { name
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
                        }) =
    "name"            := name ~>
    "description"     :=? description ~>?
    "main"            :=? main ~>?
    "moduleType"      :=? moduleType ~>?
    "license"         :=? license ~>?
    "ignore"          :=? ignore ~>?
    "keywords"        :=? keywords ~>?
    "resolutions"     :=? resolutions ~>?
    "private"         :=? private ~>?
    "dependencies"    :=? dependencies ~>?
    "devDependencies" :=? devDependencies ~>?
    "repository"      :=? repository ~>?
    "authors"         :=? authors ~>?
    "homepage"        :=? homepage

newtype Dependencies =
  Dependencies (Array { packageName :: String
                      , versionRange :: String
                      })

derive instance newtypeDependencies :: Newtype Dependencies _
derive instance eqDependencies :: Eq Dependencies
derive instance genericDependencies :: Generic Dependencies _
derive newtype instance semigroupDependencies :: Semigroup Dependencies
derive newtype instance monoidDependencies :: Monoid Dependencies
instance showDependencies :: Show Dependencies where
  show = genericShow

instance decodeJsonDependencies :: DecodeJson Dependencies where
  decodeJson json =
    Dependencies <$> fromObject <$> decodeJson json
    where
      fromObject obj =
        Object.toUnfoldable obj <#>
        \(Tuple packageName versionRange) -> { packageName, versionRange }

instance encodeJsonDependencies :: EncodeJson Dependencies where
  encodeJson =
    unwrap >>>
    map (\({ packageName, versionRange }) -> Tuple packageName versionRange) >>>
    tuplesToObjectJson

newtype Resolutions =
  Resolutions (Array { packageName :: String
                     , version :: String
                     })

derive instance newtypeResolution :: Newtype Resolutions _
derive instance eqResolutions :: Eq Resolutions
derive instance genericResolutions :: Generic Resolutions _
derive newtype instance semigroupResolutions :: Semigroup Resolutions
derive newtype instance monoidResolutions :: Monoid Resolutions
instance showResolutions :: Show Resolutions where
  show = genericShow

instance decodeJsonResolution :: DecodeJson Resolutions where
  decodeJson json =
    Resolutions <$> fromObject <$> decodeJson json
    where
      fromObject obj =
        Object.toUnfoldable obj <#>
        \(Tuple packageName version) -> { packageName, version }

instance encodeJsonResolutions :: EncodeJson Resolutions where
  encodeJson =
    unwrap >>>
    map (\({ packageName, version }) -> Tuple packageName version) >>>
    tuplesToObjectJson

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

instance encodeJsonModuleType :: EncodeJson ModuleType where
  encodeJson value =
    fromString case value of
      Globals -> "globals"
      AMD -> "amd"
      Node -> "node"
      ES6 -> "es6"
      YUI -> "yui"

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

instance encodeJsonAuthor :: EncodeJson Author where
  encodeJson (Author { name, email, homepage }) =
    "name" :=? name ~>?
    "email" :=? email ~>?
    "homepage" :=? homepage

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

tuplesToObjectJson :: forall b. EncodeJson b => Array (Tuple String b) -> Json
tuplesToObjectJson =
  foldr (\(Tuple a b) obj -> Tuple a (encodeJson b) ~> obj) jsonEmptyObject
