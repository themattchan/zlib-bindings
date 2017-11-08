{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Codec.Zlib.Lowlevel
    ( WindowBits(..)
    , defaultWindowBits
    , ZStreamStruct
    , ZStream'
    , zstreamNew
    , Strategy(..)
    , deflateInit2
    , inflateInit2
    , c_free_z_stream_inflate
    , c_free_z_stream_deflate
    , c_copy_z_stream_inflate
    , c_set_avail_in
    , c_set_avail_out
    , c_get_avail_out
    , c_get_avail_in
    , c_get_next_in
    , c_get_next_out
    , c_call_inflate_noflush
    , c_call_deflate_noflush
    , c_call_deflate_finish
    , c_call_deflate_flush
    , c_call_deflate_full_flush
    , c_call_deflate_set_dictionary
    , c_call_inflate_set_dictionary
    ) where

import Foreign.C
import Foreign.Ptr
import Data.Typeable
import GHC.Generics

data ZStreamStruct
type ZStream' = Ptr ZStreamStruct

data Strategy =
      StrategyDefault
    | StrategyFiltered
    | StrategyHuffman
    | StrategyRLE
    | StrategyFixed
    deriving (Show,Eq,Ord,Enum)

newtype WindowBits = WindowBits Int
  deriving (Eq, Ord, Show, Typeable, Generic)

defaultWindowBits :: WindowBits
defaultWindowBits = WindowBits 15

wbToInt :: WindowBits -> CInt
wbToInt (WindowBits i) = fromIntegral i

foreign import ccall unsafe "streaming_commons_create_z_stream"
    zstreamNew :: IO ZStream'

foreign import ccall unsafe "streaming_commons_deflate_init2"
    c_deflateInit2 :: ZStream' -> CInt -> CInt -> CInt -> CInt
                   -> IO ()

deflateInit2 :: ZStream' -> Int -> WindowBits -> Int -> Strategy -> IO ()
deflateInit2 zstream level windowBits memlevel strategy =
    c_deflateInit2 zstream (fromIntegral level) (wbToInt windowBits)
                   (fromIntegral memlevel)
                   (fromIntegral $ fromEnum strategy)

foreign import ccall unsafe "streaming_commons_inflate_init2"
    c_inflateInit2 :: ZStream' -> CInt -> IO ()

inflateInit2 :: ZStream' -> WindowBits -> IO ()
inflateInit2 zstream wb = c_inflateInit2 zstream (wbToInt wb)

foreign import ccall unsafe "&streaming_commons_free_z_stream_inflate"
    c_free_z_stream_inflate :: FunPtr (ZStream' -> IO ())

foreign import ccall unsafe "&streaming_commons_free_z_stream_deflate"
    c_free_z_stream_deflate :: FunPtr (ZStream' -> IO ())

foreign import ccall unsafe "streaming_commons_copy_z_stream_inflate"
    c_copy_z_stream_inflate :: ZStream' -> IO ZStream'

foreign import ccall unsafe "streaming_commons_set_avail_in"
    c_set_avail_in :: ZStream' -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "streaming_commons_set_avail_out"
    c_set_avail_out :: ZStream' -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "streaming_commons_get_avail_out"
    c_get_avail_out :: ZStream' -> IO CUInt

foreign import ccall unsafe "streaming_commons_get_avail_in"
    c_get_avail_in :: ZStream' -> IO CUInt

foreign import ccall unsafe "streaming_commons_get_next_in"
    c_get_next_in :: ZStream' -> IO (Ptr CChar)

foreign import ccall unsafe "streaming_commons_get_next_out"
    c_get_next_out :: ZStream' -> IO (Ptr CChar)

foreign import ccall unsafe "streaming_commons_call_inflate_noflush"
    c_call_inflate_noflush :: ZStream' -> IO CInt

foreign import ccall unsafe "streaming_commons_call_deflate_noflush"
    c_call_deflate_noflush :: ZStream' -> IO CInt

foreign import ccall unsafe "streaming_commons_call_deflate_finish"
    c_call_deflate_finish :: ZStream' -> IO CInt

foreign import ccall unsafe "streaming_commons_call_deflate_flush"
    c_call_deflate_flush :: ZStream' -> IO CInt

foreign import ccall unsafe "streaming_commons_call_deflate_full_flush"
    c_call_deflate_full_flush :: ZStream' -> IO CInt

foreign import ccall unsafe "streaming_commons_deflate_set_dictionary"
    c_call_deflate_set_dictionary :: ZStream' -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "streaming_commons_inflate_set_dictionary"
    c_call_inflate_set_dictionary :: ZStream' -> Ptr CChar -> CUInt -> IO ()
