{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module MelbyRenderer.Colorizer
  ( getColorizedGitSha
  ) where

import Control.Monad.Logger hiding (logDebug)
import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.CIE (lightness)
import Data.Colour.SRGB (sRGB24, toSRGB24)
import Data.Colour.RGBSpace
import Data.Either (fromRight)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Word (Word32)
import Mu.Server
import System.Console.ANSI (setSGRCode, ConsoleIntensity (..), ConsoleLayer (..), SGR (..))

import MelbyRenderer.Log (logDebug)
import MelbyRenderer.Schema

getColorizedGitSha
  :: (MonadServer m, MonadLogger m)
  => MColorizedGitShaRequest
  -> m MColorizedGitShaResponse
getColorizedGitSha req = do
  logDebug $ "request was: " <> (T.pack $ show req)
  pure $ MColorizedGitShaResponse{ sha_colorized = colorize
                                                   (sha req)
                                                   (sha_length req)
                                                   (pad_left req)
                                                   (pad_right req)}
  where
  colorize bytes len padl padr
    = renderColorized
    . (<> [(Reset, "")])
    . ((SetConsoleIntensity BoldIntensity, "") :)
    . addPadding padl padr
    $ colorizeGitSha24bit bytes len

colorizeGitSha24bit :: T.Text -> Word32 -> [(SGR, T.Text)]
colorizeGitSha24bit shaStr len
  = reverse
  . zip colorCodes
  . buildOutput []
  $ T.take (fromIntegral len) shaStr
  where
  bytes = asBytes shaStr
  -- XOR the first and second halves of the input bytes (20 bytes) to generate
  -- 10 more bytes. Now we have 30 bytes in total in bytesFinal.
  bytes1 = B.take 10 bytes
  -- FIXME: after ByteString 0.11.1.0, use B.takeEnd
  bytes2 = B.drop (B.length bytes - 10) bytes
  bytesFinal = B.concat
    -- FIXME: after ByteString 0.11.1.0, use B.packZipWith
    --[B.packZipWith (\a b -> xor a b) bytes1 bytes2, B.reverse bytes]
    [(B.pack $ B.zipWith (\a b -> xor a b) bytes1 bytes2), B.reverse bytes]
  -- Using the 30 bytes in bytesFinal, generate 10 colors (of 3 bytes, or 24 bits, each).
  colorCodes = gen10Colors [] bytesFinal
  gen10Colors acc bs = case B.length (B.take 3 bs) of
    3 -> gen10Colors ((toSgrCode . toRgbTuple $ B.take 3 bs) : acc) (B.drop 3 bs)
    _ -> acc
  toRgbTuple bs3
    = ( fromIntegral $ B.index bs3 0
      , fromIntegral $ B.index bs3 1
      , fromIntegral $ B.index bs3 2
      )
  toSgrCode (r, g, b) = SetRGBColor Background $ sRGB24 r g b
  -- Construct the output string as groups of 4 hex chars each. Use the same
  -- recursion pattern as in gen10Colors to build up an output that colorizes 4
  -- hex chars at a time.
  buildOutput acc hexChars = case T.length (T.take 4 hexChars) of
    0 -> acc
    _ -> buildOutput (T.take 4 hexChars : acc) (T.drop 4 hexChars)
addPadding :: Word32 -> Word32 -> [(SGR, T.Text)] -> [(SGR, T.Text)]
addPadding padl padr colorized = case colorized of
  [] -> []
  (c, t):[] -> [(c, prefix <> t <> suffix)]
  (c1, t1):(c2, t2):[] -> [(c1, prefix <> t1), (c2, t2 <> suffix)]
  _ -> let
    (c1, t1) = head colorized
    (c2, t2) = last colorized
    mid = init $ drop 1 colorized
    in [(c1, prefix <> t1)] <> mid <> [(c2, t2 <> suffix)]
  where
  prefix = T.replicate (fromIntegral padl) " "
  suffix = T.replicate (fromIntegral padr) " "
renderColorized :: [(SGR, T.Text)] -> T.Text
renderColorized = T.concat . map f
  where
  f (code, hexChars) = T.pack (setSGRCode [code]) <> fgColor <> hexChars
    where
    fgColor = case code of
      (SetRGBColor Background bgColor) -> ( T.pack
                                          $ setSGRCode [ SetRGBColor Foreground
                                                       $ getContrastingColor bgColor
                                                       ])
      _ -> T.empty

asBytes :: T.Text -> B.ByteString
asBytes = (fromRight B.empty . B16.decode) . T.encodeUtf8
getPerceivedLightness :: (Floating b, RealFrac b) => Colour b -> Double
getPerceivedLightness c = lightness d65 $ sRGB24 r g b
  where
  m = toSRGB24 c
  r = channelRed m
  g = channelGreen m
  b = channelBlue m

getContrastingColor :: (Floating b, RealFrac b) => Colour b -> Colour b
getContrastingColor c = if (getPerceivedLightness c) < 50.0
  then sRGB24 255 255 255
  else sRGB24 0 0 0
