module Test.Main where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Web.Bower.PackageMeta (Author(..), Dependencies(..), ModuleType(..), PackageMeta(..), Repository(..), Resolutions(..), VersionRange(..))

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

parseDecode :: forall a. DecodeJson a => String -> Either String a
parseDecode = decodeJson <=< jsonParser

main :: Effect Unit
main = runTest do
  suite "decoding" do

    test "VersionRange" do
      let expected = Right $ VersionRange "v0.0.1"
          actual = parseDecode "\"v0.0.1\""
      Assert.equal expected actual

    test "Dependencies" do
      let actual = parseDecode
                  """
                  { "purescript-foo": "^0.0.1" }
                  """
          expected = Right $ Dependencies [ { packageName: "purescript-foo"
                                            , versionRange: wrap "^0.0.1" }
                                          ]

      Assert.equal expected actual

    test "PackageMeta" do
      let actual = parseDecode bower1
          expected =
            Right $
            PackageMeta { name: "purescript-search-trie"
                        , license: Just [ "BSD-3-Clause" ]
                        , repository: Just $
                          Repository { type: "git"
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
                          { packageName: "purescript-prelude", versionRange: wrap "^4.0.0" },
                          { packageName: "purescript-arrays", versionRange: wrap "^5.0.0" },
                          { packageName: "purescript-ordered-collections", versionRange: wrap "^1.0.0" },
                          { packageName: "purescript-lists", versionRange: wrap "^5.2.0" },
                          { packageName: "purescript-foldable-traversable", versionRange: wrap "^4.0.0" },
                          { packageName: "purescript-bifunctors", versionRange: wrap "^4.0.0" }
                          ]
                        , devDependencies: Dependencies [
                          { packageName: "purescript-assert", versionRange: wrap "^4.1.0" },
                          { packageName: "purescript-psci-support", versionRange: wrap "^4.0.0" },
                          { packageName: "purescript-strings", versionRange: wrap "^4.0.0" },
                          { packageName: "purescript-effect", versionRange: wrap "^2.0.0" }
                          ]
                        , resolutions: Just $ Resolutions
                          [ { packageName: "angular"
                            , version: wrap "1.3.0-beta.16"
                            }
                          ]
                        , private: Nothing
                        }

      Assert.equal expected actual
