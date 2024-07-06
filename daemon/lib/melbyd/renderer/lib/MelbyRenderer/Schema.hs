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

module MelbyRenderer.Schema where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Generics

import Mu.Quasi.GRpc
import Mu.Schema

grpc "MelbyRendererSchema" id "lib/MelbyRenderer/melby_renderer.proto"

data MColorizedGitShaRequest = MColorizedGitShaRequest
  -- In mu-haskell, nested types must be contained in a Maybe. See
  -- https://github.com/higherkindness/mu-haskell/issues/255#issuecomment-730468011.
  { sha :: T.Text
  , sha_length :: Word32
  , pad_left :: Word32
  , pad_right :: Word32
  , render_options :: Maybe MRenderOptions
  } deriving (Eq, Show, Ord, Generic
             , ToSchema MelbyRendererSchema "ColorizedGitShaRequest"
             , FromSchema MelbyRendererSchema "ColorizedGitShaRequest")

data MColorizedGitShaResponse = MColorizedGitShaResponse
  { sha_colorized :: T.Text
  } deriving (Eq, Ord, Show, Generic
             , ToSchema MelbyRendererSchema "ColorizedGitShaResponse"
             , FromSchema MelbyRendererSchema "ColorizedGitShaResponse")
data MRenderWidgetsRequest = MRenderWidgetsRequest
  { widgets :: [MWidget]
  , delimiter :: Maybe MWidget
  -- FIXME: allow duplicate fields once we move to GHC 9 (waiting on mu-haskell)
  , render_options :: Maybe MRenderOptions
  } deriving (Eq, Show, Ord, Generic
             , ToSchema MelbyRendererSchema "RenderWidgetsRequest"
             , FromSchema MelbyRendererSchema "RenderWidgetsRequest")

data MRenderWidgetsResponse = MRenderWidgetsResponse
  { widgets_rendered :: T.Text
  } deriving (Eq, Ord, Show, Generic
             , ToSchema MelbyRendererSchema "RenderWidgetsResponse"
             , FromSchema MelbyRendererSchema "RenderWidgetsResponse")

data MWidget = MWidget
  { str :: T.Text
  , prop :: Maybe MTextProperty
  , drop_delim_left :: Bool
  , drop_delim_right :: Bool
  } deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "Widget"
           , FromSchema MelbyRendererSchema "Widget")

data MTextProperty = MTextProperty
  { fg :: Maybe MColor
  , bg :: Maybe MColor
  , styles :: [MTextStyle]
  } deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "TextProperty"
           , FromSchema MelbyRendererSchema "TextProperty")

data MColor = MColor
  { color_oneof :: MColorOneof
  } deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "Color"
           , FromSchema MelbyRendererSchema "Color")

data MColorOneof
  = MColorOneOf24Bit MColor24Bit
  | MColorOneOf256 Word32
  deriving (Eq, Ord, Show, Generic)

data MColor24Bit = MColor24Bit
  { red :: Word32
  , green :: Word32
  , blue :: Word32
  -- FIXME: add alpha channel?
  } deriving (Eq, Ord, Show, Generic
             , ToSchema MelbyRendererSchema "Color24Bit"
             , FromSchema MelbyRendererSchema "Color24Bit")

data MTextStyle
  = TEXT_STYLE_UNSPECIFIED
  | TEXT_STYLE_BOLD
  | TEXT_STYLE_ITALIC
  | TEXT_STYLE_UNDERLINE
  | TEXT_STYLE_UNDERLINE_DOUBLE
  | TEXT_STYLE_BLINK
  | TEXT_STYLE_BLINK_RAPID
  deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "TextStyle"
           , FromSchema MelbyRendererSchema "TextStyle")
data MRenderOptions = MRenderOptions
  { format :: MRenderFormat
  , color_depth :: MRenderColorDepth
  } deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "RenderOptions"
           , FromSchema MelbyRendererSchema "RenderOptions")

data MRenderFormat
  = RENDER_FORMAT_UNSPECIFIED
  | RENDER_FORMAT_UNIX_TERMINAL
  deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "RenderFormat"
           , FromSchema MelbyRendererSchema "RenderFormat")

data MRenderColorDepth
  = RENDER_COLOR_DEPTH_UNSPECIFIED
  | RENDER_COLOR_DEPTH_256
  | RENDER_COLOR_DEPTH_24_BIT
  deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "RenderColorDepth"
           , FromSchema MelbyRendererSchema "RenderColorDepth")
data MParsePathAliasesRequest = MParsePathAliasesRequest
  { path_aliases_raw :: T.Text
  } deriving (Eq, Show, Ord, Generic
             , ToSchema MelbyRendererSchema "ParsePathAliasesRequest"
             , FromSchema MelbyRendererSchema "ParsePathAliasesRequest")

data MParsePathAliasesResponse = MParsePathAliasesResponse
  { status :: MParseStatus
  , path_aliases :: M.Map T.Text T.Text
  , error :: T.Text
  } deriving (Eq, Ord, Show, Generic
             , ToSchema MelbyRendererSchema "ParsePathAliasesResponse"
             , FromSchema MelbyRendererSchema "ParsePathAliasesResponse")

data MParseStatus
  = PARSE_STATUS_UNSPECIFIED
  | PARSE_STATUS_ERROR
  | PARSE_STATUS_OK
  deriving (Eq, Ord, Show, Generic
           , ToSchema MelbyRendererSchema "ParseStatus"
           , FromSchema MelbyRendererSchema "ParseStatus")
