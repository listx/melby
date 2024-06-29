-- Copyright 2024 Linus Arver
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE FlexibleContexts #-}

module MelbyRenderer.PathAliases
  ( parsePathAliases
  , pPathAliases
  , pPathAlias
  ) where

import Control.Applicative hiding (many, some)
import Data.Map.Strict as M
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAsciiUpper, isAsciiLower, isDigit, isPunctuation, isAlphaNum)
import Mu.Server (MonadServer)
import Control.Monad.Logger hiding (logDebug)

import MelbyRenderer.Log (logDebug)
import MelbyRenderer.Schema

type Megaparser = Parsec Void T.Text

parsePathAliases
  :: (MonadServer m, MonadLogger m)
  => MParsePathAliasesRequest
  -> m MParsePathAliasesResponse
parsePathAliases req = do
  logDebug $ "request was: " <> (T.pack $ show req)
  let
    result = parsePathAliasesRaw $ path_aliases_raw req
  case result of
    Right pathAliases -> pure $ MParsePathAliasesResponse
      { status = PARSE_STATUS_OK
      , path_aliases = pathAliases
      , error = ""
      }
    Left err -> pure $ MParsePathAliasesResponse
      { status = PARSE_STATUS_ERROR
      , path_aliases = M.empty
      , error = err
      }

parsePathAliasesRaw :: T.Text -> Either T.Text (M.Map T.Text T.Text)
parsePathAliasesRaw input = case parse pPathAliases "" input of
  Left err -> Left . T.pack $ errorBundlePretty err
  Right m -> Right m

pPathAliases :: Megaparser (M.Map T.Text T.Text)
pPathAliases = do
  -- Clear leading whitespace/comments.
  _ <- optional sc
  pathAliasesAsTuples <- Text.Megaparsec.many pPathAlias
  eof
  pure $ M.fromList pathAliasesAsTuples

pPathAlias :: Megaparser (T.Text, T.Text)
pPathAlias = do
  -- Example of the kind of line we want to parse:
  --    hash -d   kt=${HOME}/go/src/k8s.io/test-infra
  _ <- symbol "hash"
  _ <- symbol "-d"
  alias <- pAlias
  _ <- char '='
  path <- T.concat <$> lexeme (directoryPath <?> "directory path")
  -- We want to have the path as the key to the Map, because that's how we use
  -- it back in Elixir (FIXME: add link).
  pure (path, alias)
  -- FIXME: Use withRecovery to recover from a parse error. See
  -- https://stackoverflow.com/questions/59640023/how-to-report-multiple-errors-using-megaparsec.
  --if isValid path
    --then pure Just (path, alias)
    --else pure Nothing
  where
  directoryPath = do
    dp <- some $ choice [pPath, pVariable]
    -- This is a hacky way of checking cases where we should fail when the user
    -- has put in an aberrant "$" symbol (when it is *not* used as part of
    -- defining an environment variable like "${FOO}".) This is the only way we
    -- can expect a kind of "eof" at the end here without actually using "eof"
    -- (we can't use eof because this parser is part of a larger parser that
    -- uses eof, namely pPathAliases). We would ideally want to use an eof here
    -- because we want this parser to consume all non-space characters while
    -- still playing nicely with the lexeme which consumes all trailing comments
    -- and newlines.

    -- In other words, this checks the case where we have a badly formed path
    -- like "/clam-chowder-$oup" --- here the "/clam-chowder-" will get
    -- successfully parsed, and neither pVariable nor pPath will succeed on the
    -- remaining "$oup" input. We want to ensure that whatever we end up
    -- parsing, we will *NOT* get a dollar symbol immediately following a
    -- successful parse.
    notFollowedBy (char '$')
    pure dp

pAlias :: Megaparser T.Text
pAlias = do
  firstLetter <- satisfy isAllowedFirstLetter <?> "first letter of alias"
  rest <- takeWhileP (Just "alias chars") isAllowedTrailingLetter
  pure $ T.cons firstLetter rest
  where
  -- The equivalent regex for the allowed range is "_A-Za-z0-9".
  isAllowedFirstLetter c
    = or $ Prelude.map ($ c)
      [ isAsciiLower
      , isAsciiUpper
      , isDigit
      , (== '_')
      ]
  -- Same as isAllowedFirstLetter, but also allow dashes. Note that these
  -- parsing rules is just a reflection of what is normally allowed by Zsh's
  -- "hash" builtin function. There, if we try something like "hash -d
  -- -myalias=/a/very/long/path", the "-myalias" gets interpreted as a flag to
  -- the "hash" function. This is why we don't allow the dash character to be
  -- used as the first letter of the alias.
  isAllowedTrailingLetter c = isAllowedFirstLetter c || c == '-'

pPath :: Megaparser T.Text
pPath = takeWhile1P (Just "directory chars") isAllowedPathChar
  where
  -- Technically speaking, a folder can have a literal "$" character in it, like
  -- this: "/foo/bar/\$baz", but in order to simplify our implementation we
  -- forbid them because they are too rare. So, we just prohibit the dollar
  -- symbol, because it is the first expected character of a variable.
  isAllowedPathChar c = (c /= '$') && (isAlphaNum c || isPunctuation c)

pVariable :: Megaparser T.Text
pVariable = do
  _ <- string "${" <?> "dollar-and-opening-brace"
  firstLetter <- satisfy isAllowedFirstLetter <?> "first letter of shell variable name"
  rest <- takeWhile1P (Just "shell variable name chars") isAllowedTrailingLetter
  _ <- char '}'
  pure $ "${" <> T.cons firstLetter rest <> "}"
  where
  isAllowedFirstLetter c
    | isAsciiUpper c = True
    | c == '_' = True
    | otherwise = False
  isAllowedTrailingLetter c = isAllowedFirstLetter c || isDigit c

-- This is taken from https://markkarpov.com/tutorial/megaparsec.html#lexing. We
-- want to use these lexeme-based helpers to create parsers that ignore both
-- whitespace as well as comment lines. Othwerwise, we'd have to manually
-- construct these parsers as well, which is annoying and too low-level for us.
sc :: Megaparser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  Text.Megaparsec.empty

lexeme :: Megaparser a -> Megaparser a
lexeme = L.lexeme sc

symbol :: T.Text -> Megaparser T.Text
symbol = L.symbol sc
