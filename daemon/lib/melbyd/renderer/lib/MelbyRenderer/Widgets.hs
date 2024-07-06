{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module MelbyRenderer.Widgets
  ( renderWidgets
  ) where

import Control.Monad.Logger hiding (logDebug)
import Data.Colour.SRGB (sRGB24)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Text as T
import GHC.Records (getField)
import Mu.Server
import System.Console.ANSI
       ( setSGRCode
       , BlinkSpeed (..)
       , ConsoleIntensity (..)
       , ConsoleLayer (..)
       , SGR (..)
       , Underlining (..))

import MelbyRenderer.Log (logDebug)
import MelbyRenderer.Schema

renderWidgets
  :: (MonadServer m, MonadLogger m)
  => MRenderWidgetsRequest
  -> m MRenderWidgetsResponse
renderWidgets req = do
  logDebug $ "request was: " <> (T.pack $ show req)
  pure $ MRenderWidgetsResponse
    { widgets_rendered = rendered
    }
  where
  rendered = T.concat $ map (renderWidget renderOpts) widgetsWithDelimiters
  widgetsWithDelimiters = injectDelimiters delimiterFinal $ widgets req
  delimiterFinal = case delimiter req of
    Just d -> d
    Nothing -> MWidget
      { str = " "
      , prop = Nothing
      , drop_delim_left = False
      , drop_delim_right = False
      }
  renderOpts = case GHC.Records.getField @"render_options" req of
    Just ro -> ro
    Nothing -> MRenderOptions
      { format = RENDER_FORMAT_UNIX_TERMINAL
      , color_depth = RENDER_COLOR_DEPTH_24_BIT
      }

injectDelimiters :: MWidget -> [MWidget] -> [MWidget]
injectDelimiters delim ws = case ws of
  [] -> []
  [w] -> [w]
  (w1:w2:rest) -> let recurse = injectDelimiters delim (w2:rest)
    in if drop_delim_right w1 || drop_delim_left w2
      then w1:recurse
      else w1:delim:recurse

renderWidget :: MRenderOptions -> MWidget -> T.Text
renderWidget renderOptions widget = case format renderOptions of
  RENDER_FORMAT_UNSPECIFIED -> "RENDER_FORMAT_UNSPECIFIED is unimplemented"
  RENDER_FORMAT_UNIX_TERMINAL -> T.concat
    [ propCodes
    , MelbyRenderer.Schema.str widget
    , T.pack $ setSGRCode [Reset]
    ]
  where
  propCodes = case prop widget of
    Just p -> textPropToAnsiCode p
    Nothing -> ""

textPropToAnsiCode :: MTextProperty -> T.Text
textPropToAnsiCode tp
  = T.pack . setSGRCode $ catMaybes
    ([ propFg
     , propBg
     ] <> map (listToMaybe . styleToSGRCode) (styles tp))
  where
  propFg = case fg tp of
    Nothing -> Nothing
    Just mcolor -> colorToSGRCode mcolor Foreground
  propBg = case bg tp of
    Nothing -> Nothing
    Just mcolor -> colorToSGRCode mcolor Background
  colorToSGRCode mcolor fgOrBg = case color_oneof mcolor of
    -- FIXME: Get the default terminal background color and use it to blend the
    -- foreground color here against it to get a Colour (instead of
    -- AlphaColour).

    -- (MColorOneOf24Bit mcolor24bit) -> Just
    --                                . SetRGBColor Foreground
    --                                . withOpacity (fromIntegral $ alpha mcolor24bit)
    (MColorOneOf24Bit mcolor24bit) -> Just . SetRGBColor fgOrBg
      $ sRGB24 (fromIntegral $ red mcolor24bit)
               (fromIntegral $ green mcolor24bit)
               (fromIntegral $ blue mcolor24bit)
    (MColorOneOf256 mcolor256) -> Just $ SetPaletteColor fgOrBg (fromIntegral mcolor256)
  styleToSGRCode = \case
    TEXT_STYLE_UNSPECIFIED -> []
    TEXT_STYLE_BOLD -> [SetConsoleIntensity BoldIntensity]
    TEXT_STYLE_ITALIC -> [SetItalicized True]
    TEXT_STYLE_UNDERLINE -> [SetUnderlining SingleUnderline]
    TEXT_STYLE_UNDERLINE_DOUBLE -> [SetUnderlining DoubleUnderline]
    TEXT_STYLE_BLINK -> [SetBlinkSpeed SlowBlink]
    TEXT_STYLE_BLINK_RAPID -> [SetBlinkSpeed RapidBlink]
