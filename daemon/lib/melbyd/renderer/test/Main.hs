{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Map.Strict as M
import NeatInterpolation
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import MelbyRenderer.PathAliases (pPathAlias, pPathAliases)

main :: IO ()
main = hspec $ do
  describe "pPathAlias" $ do
    -- Good cases.
    context "when parsing a typical case" $
      it "works" $
        parse pPathAlias "" "hash -d foo=/bar" `shouldParse` ("/bar", "foo")
    context "when parsing a typical case with extra spaces" $
      it "works" $
        parse pPathAlias "" "hash   -d     foo=/bar   " `shouldParse` ("/bar", "foo")
    context "when parsing a path with an embedded variable" $
      it "works" $
        parse pPathAlias "" "hash -d   kk=${HOME}/go/src/k8s.io/kubernetes"
          `shouldParse` ("${HOME}/go/src/k8s.io/kubernetes", "kk")
    context "when parsing a path with multiple embedded variables" $
      it "works" $
        parse pPathAlias "" "hash -d   x=${HOME}/foo/${SUBDIR}/baz"
          `shouldParse` ("${HOME}/foo/${SUBDIR}/baz", "x")
    context "when parsing a path with a trailing comment" $
      it "works" $
        parse pPathAlias "" "hash -d   x=${HOME}/hello # Comment text"
          `shouldParse` ("${HOME}/hello", "x")
    -- Bad cases.
    context "when the equal sign has a space to the left" $
      it "fails" $
        parse pPathAlias "" "hash   -d     foo =/bar   "
          `shouldFailWith` err 17 (utok ' ' <> etok '=' <> elabel "alias chars")
    context "when the equal sign has a space to the right" $
      it "fails" $
        parse pPathAlias "" "hash   -d     foo= /bar   "
          `shouldFailWith` err 18 (utoks " /" <> elabel "directory path")
    context "when there is an aberrant dollar sign in the path" $
      it "fails" $
        parse pPathAlias "" "hash -d x=/clam-chowder-$oup\n"
          `shouldFailWith` err 24 (utok '$' <> elabel "directory chars"
                                            <> elabel "dollar-and-opening-brace")
  describe "pPathAliases" $ do
    context "when there are comment lines" $
      it "works" $
        let
          x =
            [trimming|

            # Comment at beginning of file.

            hash -d x=/foo # Trailing comment.
            # Commented out declaration.
            # hash -d x=/foo

            # Unfortunately we have to escape the dollar sign to make it play
            # nicely with Template Haskell.
            hash -d kk=$${HOME}/go/src/k8s.io/kubernetes

            # Indented entry.
              hash    -d   y=/hello
            # Comment at end of file.
            |]
        in parse pPathAliases "" x
           `shouldParse` M.fromList
             [ ("/foo", "x")
             , ("${HOME}/go/src/k8s.io/kubernetes", "kk")
             , ("/hello", "y")]
