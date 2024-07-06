{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DerivingVia           #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLabels      #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}

module MelbyClient.Schema where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import GHC.Generics

import Mu.Quasi.GRpc
import Mu.Schema

grpc "MelbyClientSchema" id "lib/MelbyClient/melby_client.proto"

-- The "M" prefix for the types here is for "Message" as in a gRPC "message"
-- type.

data MViewRequest = MViewRequest
  { config_path :: T.Text
  , config :: T.Text
  , env_vars :: M.Map T.Text T.Text
  , shell_pid :: T.Text
  } deriving (Eq, Show, Ord, Generic
             , ToSchema MelbyClientSchema "ViewRequest"
             , FromSchema MelbyClientSchema "ViewRequest")

data MViewResponse = MViewResponse
  { status :: MViewStatus
  , view :: T.Text
  , error :: T.Text
  } deriving (Eq, Ord, Show, Generic
             , ToSchema MelbyClientSchema "ViewResponse"
             , FromSchema MelbyClientSchema "ViewResponse")

data MViewStatus
  = VIEW_STATUS_UNSPECIFIED
  | VIEW_STATUS_ERROR
  | VIEW_STATUS_OK
  deriving (Eq, Ord, Show, Generic
             , ToSchema MelbyClientSchema "ViewStatus"
             , FromSchema MelbyClientSchema "ViewStatus")
