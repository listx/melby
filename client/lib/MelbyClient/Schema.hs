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
