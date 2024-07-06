{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- It's funny that we have to specify this, when we already declare
-- PartialTypeSignatures above.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Control.Monad.Logger hiding (logDebug)
import Data.Version (showVersion)
import Mu.GRpc.Server
import Mu.Server
import Options.Applicative
import System.Environment (lookupEnv)

import Paths_melby_renderer (version)
import MelbyRenderer.Colorizer (getColorizedGitSha)
import MelbyRenderer.GitVersion
import MelbyRenderer.Schema
import MelbyRenderer.PathAliases (parsePathAliases)
import MelbyRenderer.Widgets (renderWidgets)
newtype Opts = Opts
  { subcommand :: Subcommand }

data Subcommand
  = Serve ServeOpts
  | Ping

data ServeOpts = ServeOpts
  { oPort :: Int
  }
optionsP :: Parser Opts
optionsP = Opts <$> subcommandP

subcommandP :: Parser Subcommand
subcommandP = hsubparser
  (  command "serve" (info (Serve <$> serveOptsP) (progDesc "get prompt information with Lua"))
  <> command "ping" (info (pure Ping) (progDesc "FIXME ?"))
  <> metavar "SUBCOMMAND"
  )

serveOptsP :: Parser ServeOpts
serveOptsP
  = ServeOpts
  <$> (argument auto (metavar "PORT"))

optsHandler :: Opts -> IO ()
optsHandler (Opts subcommand') = do
  case subcommand' of
    Serve o -> do
      putStrLn $ "serving over port " <> (show (oPort o))
      mixEnv <- lookupEnv "MIX_ENV"
      let
        logFilter = case mixEnv of
          -- For production, disable LevelDebug logs.
          Just "prod" -> (\_logSource logLevel -> logLevel > LevelInfo)
          _ -> (\_ _ -> True)
      runGRpcAppTrans msgProtoBuf (oPort o) (runStderrLoggingT . filterLogger logFilter) renderer
    Ping -> putStrLn "not implemented"

renderer :: (MonadServer m, MonadLogger m) => SingleServerT info Renderer m _
renderer = singleService
  ( method @"GetColorizedGitSha" getColorizedGitSha
  , method @"RenderWidgets" renderWidgets
  , method @"ParsePathAliases" parsePathAliases
  )
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
    <> header "melbyr - melby renderer"
  versionOption = infoOption
    (concat [showVersion version, "-g", $(gitVersion)])
    (long "version" <> short 'v' <> help "Show version")
