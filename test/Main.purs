module Test.Main where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Web.Bower.PackageMeta (Author(..), Dependencies(..), ModuleType(..), BowerJson(..), Resolutions(..), PackageMeta(..))

bower1 :: String
bower1 = """
{
  "name": "purescript-search-trie",
  "description": "some description",
  "license": "BSD-3-Clause",
  "main": [
    "js/motion.js",
    "sass/motion.scss"
  ],
  "moduleType": "amd",
  "repository": {
    "type": "git",
    "url": "git://github.com/klntsky/purescript-search-trie.git"
  },
  "ignore": [
    "**/.*",
    "node_modules",
    "bower_components",
    "output"
  ],
  "authors": [ "name1", { "name": "name2", "email": "email", "homepage": "homepage" } ],
  "keywords": ["k1", "k2"],
  "dependencies": {
    "purescript-prelude": "^4.0.0",
    "purescript-arrays": "^5.0.0",
    "purescript-ordered-collections": "^1.0.0",
    "purescript-lists": "^5.2.0",
    "purescript-foldable-traversable": "^4.0.0",
    "purescript-bifunctors": "^4.0.0"
  },
  "devDependencies": {
    "purescript-assert": "^4.1.0",
    "purescript-psci-support": "^4.0.0",
    "purescript-strings": "^4.0.0",
    "purescript-effect": "^2.0.0"
  },
  "resolutions": {
    "angular": "1.3.0-beta.16"
  },
  "homepage": "homepage"
}
"""

bower1Data :: BowerJson
bower1Data =
  BowerJson { name: "purescript-search-trie"
            , license: Just [ "BSD-3-Clause" ]
            , repository: Just  { type: "git"
                                , url: "git://github.com/klntsky/purescript-search-trie.git"
                                }
            , description: Just "some description"
            , main: Just [ "js/motion.js"
                         , "sass/motion.scss"
                         ]
            , moduleType: Just [ AMD ]
            , ignore:
              Just [ "**/.*"
                   , "node_modules"
                   , "bower_components"
                   , "output"
                   ]
            , keywords: Just [ "k1", "k2" ]
            , authors: Just [ Author { name: Just "name1"
                                     , email: Nothing
                                     , homepage: Nothing
                                     }
                            , Author { name: Just "name2"
                                     , email: Just "email"
                                     , homepage: Just "homepage"
                                     }
                            ]
            , homepage: Just "homepage"
            , dependencies: Dependencies [
              { packageName: "purescript-prelude", versionRange: "^4.0.0" },
              { packageName: "purescript-arrays", versionRange: "^5.0.0" },
              { packageName: "purescript-ordered-collections", versionRange: "^1.0.0" },
              { packageName: "purescript-lists", versionRange: "^5.2.0" },
              { packageName: "purescript-foldable-traversable", versionRange: "^4.0.0" },
              { packageName: "purescript-bifunctors", versionRange: "^4.0.0" }
              ]
            , devDependencies: Dependencies [
              { packageName: "purescript-assert", versionRange: "^4.1.0" },
              { packageName: "purescript-psci-support", versionRange: "^4.0.0" },
              { packageName: "purescript-strings", versionRange: "^4.0.0" },
              { packageName: "purescript-effect", versionRange: "^2.0.0" }
              ]
            , resolutions: Just $ Resolutions
              [ { packageName: "angular"
                , version: "1.3.0-beta.16"
                }
              ]
            , private: Nothing
            }

packageMeta1 :: PackageMeta
packageMeta1 = PackageMeta { name: "..."
                           , description: Nothing
                           , main: []
                           , moduleType: []
                           , license: []
                           , ignore: []
                           , keywords: []
                           , resolutions: mempty
                           , private: false
                           , dependencies: mempty
                           , devDependencies: mempty
                           , repository: Nothing
                           , authors: []
                           , homepage: Nothing
                           }

bowerJson2 :: BowerJson
bowerJson2 =
  BowerJson { name: "..."
            , license: Nothing
            , repository: Nothing
            , description: Nothing
            , main: Nothing
            , moduleType: Nothing
            , ignore: Nothing
            , keywords: Nothing
            , authors: Nothing
            , homepage: Nothing
            , dependencies: mempty
            , devDependencies: mempty
            , resolutions: Nothing
            , private: Nothing
            }

main :: Effect Unit
main = runTest do
  suite "decoding" do

    suite "Author" do
      test "decodeJson #1" do
        let actual = parseDecode
                     """
                     "author"
                     """
            expected = Right $ Author { name: Just "author", email: Nothing, homepage: Nothing }
        Assert.equal expected actual

      test "decodeJson #2" do
        let actual = parseDecode
                     """
                     { "name": "author", "homepage": "..." }
                     """
            expected = Right $ Author { name: Just "author", email: Nothing, homepage: Just "..." }
        Assert.equal expected actual

    suite "Dependencies" do

      test "decodeJson" do
        let actual = parseDecode
                     """
                     { "purescript-foo": "^0.0.1" }
                     """
            expected = Right $ Dependencies [ { packageName: "purescript-foo"
                                              , versionRange: "^0.0.1" }
                                            ]
        Assert.equal expected actual

      test "encodeJson" do
        let deps = Dependencies [ { packageName: "foo", versionRange: "v0.0.1" } ]
            actual = decodeJson $ encodeJson deps
            expected = Right deps
        Assert.equal expected actual

    suite "BowerJson" do

      test "decodeJson" do
        let actual = parseDecode bower1
            expected = Right bower1Data
        Assert.equal expected actual

      test "encodeJson" do
        let actual = decodeJson $ encodeJson bower1
            expected = Right bower1
        Assert.equal expected actual

    suite "PackageMeta" do
      test "decodeJson" do
        let actual = decodeJson $ encodeJson packageMeta1
            expected = Right bowerJson2
        Assert.equal expected actual

parseDecode :: forall a. DecodeJson a => String -> Either String a
parseDecode = decodeJson <=< jsonParser
