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

{-# language DataKinds #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}

module Main where

import Control.Monad (filterM, when)
import Data.List (isSuffixOf, nub)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Data.Time.Clock
import GHC.IO.Handle
import NeatInterpolation hiding (text)
import Options.Applicative
import System.Directory (doesFileExist)
import System.FilePath
import System.Process

import Data.Version (showVersion)
import Paths_ptu (version)
import PostTangleUtil.GitVersion
getSourceCodeFiles :: FilePath -> IO [(FilePath, T.Text)]
getSourceCodeFiles projRoot = do
  allFiles <- getAllTrackedFiles projRoot
  contents <- mapM (\path -> T.readFile path) allFiles
  pure
    . filter (\(path, content) ->
                 hasExtension' path extensions || hasShebang content)
    $ zip allFiles contents
  where
  extensions = map fst supportedExtensions

getAllTrackedFiles :: FilePath -> IO [FilePath]
getAllTrackedFiles projRoot = do
  (_, Just hout, _, _) <- createProcess
    (proc "git"
      [ "-C"
      , projRoot
      , "ls-tree"
      , "-r"
      , "--name-only"
      , "--full-tree"
      , "HEAD"])
    { std_out = CreatePipe
    }
  relativePaths <- lines <$> hGetContents hout
  let absolutePaths = map (\p -> projRoot </> p) relativePaths
  filterM doesFileExist absolutePaths

hasExtension' :: FilePath -> [String] -> Bool
hasExtension' path exts = any (flip isSuffixOf path) exts

hasShebang :: T.Text -> Bool
hasShebang text = case (take 1 $ T.lines text) of
  [firstLine] -> T.isPrefixOf "#!/" firstLine
  _ -> False

supportedExtensions :: [(String, T.Text)]
supportedExtensions =
  [ (".cabal", "--")
  , (".el", ";;")
  , (".ex", "#")
  , (".exs", "#")
  , (".ghci", "--")
  , (".hs", "--")
  , (".lua", "--")
  , (".nix", "#")
  , (".proto", "//")
  , (".org", "#")
  , (".service", "#") -- systemd unit files
  , (".sh", "#")
  , (".toml", "#")
  , (".yaml", "#")
  , ("Makefile", "#")
  ]
prependCopyrightHeaders :: FilePath -> T.Text -> IO ()
prependCopyrightHeaders projRoot author = do
  sourceCodeFiles <- getSourceCodeFiles projRoot
  mapM_ (prependCopyrightHeader author) sourceCodeFiles

prependCopyrightHeader :: T.Text -> (FilePath, T.Text) -> IO ()
prependCopyrightHeader author (path, content) = do
  putStrLn $ "ptu: processing " <> path
  -- Skip processing if the file has copyright text in it already.
  when (isNothing $ copyrightLocation content) $ do
    (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
    let ext = takeExtension path
        boilerplate = copyrightApache (T.pack $ show year) author
        copyrightHeader = mkCopyrightHeader ext boilerplate underShebang
        (shebang, underShebang) = extractShebang content
    if underShebang
       then T.writeFile path (shebang <> "\n" <> copyrightHeader <> rest content)
       else T.writeFile path (copyrightHeader <> "\n" <> content)
  where
  extractShebang text = case (take 1 $ T.lines text) of
    [firstLine] -> (firstLine, T.isPrefixOf "#!/" firstLine)
    _ -> ("", False)
  rest text = T.unlines . drop 1 $ T.lines text

data CopyrightLocation
  = CopyrightLocationBeginning
  | CopyrightLocationAfterShebang
  deriving (Enum, Eq, Ord, Show)

copyrightLocation :: T.Text -> Maybe CopyrightLocation
copyrightLocation text
  | existsAtBeginnig = Just CopyrightLocationBeginning
  | existsAfterShebang = Just CopyrightLocationAfterShebang
  | otherwise = Nothing
  where
  commentStrsToCheck = nub $ map snd supportedExtensions
  existsAtBeginnig
    = any (\commentStr -> hasCopyrightLine commentStr text) commentStrsToCheck
  existsAfterShebang = case (take 3 $ asLines) of
    [a,b,c] -> all (==True)
      [ hasShebangLine a
      , any (\commentStr -> commentStr == b && hasCopyrightLine commentStr c)
            commentStrsToCheck
      ]
    _ -> False
  asLines = T.lines text
  hasShebangLine s = T.isPrefixOf "#!/" s
  hasCopyrightLine p s = T.isPrefixOf (p <> " Copyright") s

mkCopyrightHeader :: String -> T.Text -> Bool -> T.Text
mkCopyrightHeader ext boilerplate underShebang
  | underShebang = "#\n" <> copyrightBlock <> "\n"
  | otherwise = copyrightBlock
  where
  prependPrefix prefix' line
    | T.null line = prefix'
    | otherwise = T.append (prefix' <> " ") line
  prefix = case (lookup ext supportedExtensions) of
    Just p -> p
    Nothing -> "#"
  copyrightBlock = T.unlines . map (prependPrefix prefix) $ T.lines boilerplate

copyrightApache :: T.Text -> T.Text -> T.Text
copyrightApache year author =
  [trimming|
    Copyright $year $author

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

         http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
  |]
newtype Opts = Opts
  { subcommand :: Subcommand }

data Subcommand
  = AddLegal AddLegalOpts

data AddLegalOpts = AddLegalOpts
  { oProjRoot :: FilePath
  , oAuthor :: T.Text
  }
optionsP :: Parser Opts
optionsP = Opts <$> subcommandP

subcommandP :: Parser Subcommand
subcommandP = hsubparser
  ( command "add-legal"
    (info (AddLegal <$> addLegalOptsP)
          (progDesc "set copyright headers in source files for a project"))
  <> metavar "SUBCOMMAND"
  )

addLegalOptsP :: Parser AddLegalOpts
addLegalOptsP
  = AddLegalOpts
  <$> (argument str (metavar "PROJECT_ROOT"))
  <*> (argument str (metavar "AUTHOR"))
optsHandler :: Opts -> IO ()
optsHandler (Opts subcommand') = do
  case subcommand' of
    AddLegal o -> do
      putStrLn "ptu: Prepending copyright headers."
      prependCopyrightHeaders (oProjRoot o) (oAuthor o)
      putStrLn "ptu: Done."
main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) optsP
  optsHandler opts
  where
  optsP = info parserProgramOptions infoMod
  parserProgramOptions = helper
    <*> versionOption
    <*> optionsP
  infoMod = fullDesc
    <> header "ptu - post-tangle utility"
  versionOption = infoOption
    (concat [showVersion version, "-g", $(gitVersion)])
    (long "version" <> short 'v' <> help "Show version")
