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
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}

module Main where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Version (showVersion)
import GHC.OverloadedLabels (fromLabel)
import GHC.Records (getField)
import Mu.GRpc.Client.Optics
import Mu.Schema (fromSchema, toSchema)
import Network.Socket (PortNumber)
import Options.Applicative
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import System.IO (stderr)

import Paths_melby_client (version)
import MelbyClient.GitVersion
import MelbyClient.Schema
getView :: GRpcConnection View 'MsgProtoBuf -> FilePath -> T.Text -> Int -> IO ()
getView conn configPath' config' shell_pid' = do
  env_vars_str <- getEnvironment
  let env_vars' = M.fromList $ map (\(k, v) -> (T.pack k, T.pack v)) env_vars_str
      req = toSchema $ MViewRequest
        { config_path = T.pack configPath'
        , config = config'
        , env_vars = env_vars'
        , shell_pid = T.pack $ show shell_pid'
        }
  response <- conn ^. fromLabel @"GetView" $ req
  let term = case response of
        GRpcOk a -> Right a
        x -> Left $ T.pack ("unrecognized server response: " <> (show x))
  case term of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right a -> do
      let viewResponse = fromSchema a :: MViewResponse
      case status viewResponse of
        VIEW_STATUS_UNSPECIFIED -> do
          T.hPutStrLn stderr ("VIEW_STATUS_UNSPECIFIED")
          exitFailure
        VIEW_STATUS_ERROR -> do
          T.hPutStrLn stderr ("encountered error: " <> getField @"error" viewResponse)
          exitFailure
        VIEW_STATUS_OK -> do
          T.putStrLn $ getField @"view" viewResponse
data Opts = Opts
  { subcommand :: Subcommand
  , oMelbydPort :: PortNumber
  }

data Subcommand
  = View ViewOpts
  | Ping
  | Shutdown

data ViewOpts = ViewOpts
  { oConfigPath :: FilePath
  , oConfig :: T.Text
  , oShellPid :: Int
  }
optionsP :: Parser Opts
optionsP = Opts
  <$> subcommandP
  <*> (option auto (long "melbyd-port"
                    <> help "port for melbyd (e.g., 50052 for dev environmont))"
                    <> showDefault <> value 50051 <> metavar "PORT"))

subcommandP :: Parser Subcommand
subcommandP = hsubparser
  (  command "view" (info
                     (View <$> viewOptsP)
                     (progDesc "get prompt information with Lua"))
  <> command "ping" (info
                     (pure Ping) (progDesc "Check lh server connectivity"))
  <> command "shutdown" (info
                         (pure Shutdown)
                         (progDesc "Shut down lh server instance"))
  <> metavar "SUBCOMMAND"
  )

viewOptsP :: Parser ViewOpts
viewOptsP
  = ViewOpts
  <$> (argument str (metavar "FILEPATH"))
  <*> (option auto (long "config"
                    <> help "raw Lua string" <> showDefault <> value ""
                    <> metavar "LUA_CONFIG_RAW"))
  <*> (option auto (long "shell-pid"
                    <> help "PID of invoking shell (in most shells this is '$$')"
                    <> showDefault <> value 0 <> metavar "PID"))

optsHandler :: Opts -> IO ()
optsHandler (Opts subcommand' oMelbydPort') = do
  -- FIXME: configure the domain and port to be configurable through TOML or
  -- yaml in a shared location with the server. The server should configure its
  -- port with it. The precedence of settings (where later mechanisms override
  -- earlier ones) is: (1) the configuration file (in TOML) converted to a
  -- native Haskell type (with sane defaults) with tomland
  -- (https://kowainik.github.io/posts/2019-01-14-tomland), (2) command-line
  -- options that are passed in at runtime.
  Right viewClient <- initGRpc (grpcClientConfigSimple
                                 "127.0.0.1" oMelbydPort' False) msgProtoBuf
  --home <- T.pack <$> getEnv "HOME"
  case subcommand' of
    View o -> do
      getView viewClient (oConfigPath o) (oConfig o) (oShellPid o)
    Ping -> putStrLn "not implemented"
    Shutdown -> putStrLn "not implemented"
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
    <> header "melbyc - CLI for interacting with the melby server (melbyd)"
  versionOption = infoOption
    (concat [showVersion version, "-g", $(gitVersion)])
    (long "version" <> short 'v' <> help "Show version")
