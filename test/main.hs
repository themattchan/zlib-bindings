{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..))

import Data.Streaming.Zlib
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Control.Monad (foldM, forM_, forM)
import System.IO.Unsafe (unsafePerformIO)
import Codec.Compression.Zlib hiding (WindowBits, defaultWindowBits)
import qualified Codec.Compression.GZip as Gzip
import qualified Codec.Compression.Zlib.Raw as Raw

runPopper :: ([S8.ByteString] -> c) -> IO PopperRes -> IO ([S8.ByteString] -> c)
runPopper front x = do
    y <- x
    case y of
      PRDone -> return front
      PRError _ -> return front
      PRNext z -> runPopper (front . (:) z) x

decompress' :: L.ByteString -> L.ByteString
decompress' gziped = unsafePerformIO $ do
    inf <- initInflate defaultWindowBits
    ungziped <- foldM (go' inf) id $ L.toChunks gziped
    final <- finishInflate inf
    return $ L.fromChunks $ ungziped [final]
  where
    go' inf front bs = feedInflate inf bs >>= runPopper front

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks `fmap` arbitrary
instance Arbitrary S.ByteString where
    arbitrary = S.pack `fmap` arbitrary

compress' :: L.ByteString -> L.ByteString
compress' raw = unsafePerformIO $ do
    def <- initDeflate 7 defaultWindowBits
    gziped <- foldM (go' def) id $ L.toChunks raw
    gziped' <- runPopper gziped $ finishDeflate def
    return $ L.fromChunks $ gziped' []
  where
    go' def front bs = feedDeflate def bs >>= runPopper front

license :: S.ByteString
license = S8.filter (/= '\r') $ unsafePerformIO $ S.readFile "LICENSE"

exampleDict :: S.ByteString
exampleDict = "INITIALDICTIONARY"

deflateWithDict :: S.ByteString -> L.ByteString -> L.ByteString
deflateWithDict dict raw = unsafePerformIO $ do
    def <- initDeflateWithDictionary 7 dict $ WindowBits 15
    compressed <- foldM (go' def) id $ L.toChunks raw
    compressed' <- runPopper compressed $ finishDeflate def
    return $ L.fromChunks $ compressed' []
  where
    go' def front bs = feedDeflate def bs >>= runPopper front

inflateWithDict :: S.ByteString -> L.ByteString -> L.ByteString
inflateWithDict dict compressed = unsafePerformIO $ do
    inf <- initInflateWithDictionary (WindowBits 15) dict
    decompressed <- foldM (go' inf) id $ L.toChunks compressed
    final <- finishInflate inf
    return $ L.fromChunks $ decompressed [final]
  where
    go' inf front bs = feedInflate inf bs >>= runPopper front

main :: IO ()
main = hspec $ do
    describe "inflate/deflate" $ do
        prop "decompress'" $ \lbs -> lbs == decompress' (compress lbs)
        prop "compress'" $ \lbs -> lbs == decompress (compress' lbs)

        prop "with dictionary" $ \bs ->
            bs ==
            (inflateWithDict exampleDict . deflateWithDict exampleDict) bs
        it "different dict" $ do
            raw <- L.readFile "LICENSE"
            deflated <- return $ deflateWithDict exampleDict raw
            inflated <- return $ inflateWithDict (S.drop 1 exampleDict) deflated
            inflated `shouldSatisfy` L.null

    describe "license" $ do
        it "single deflate" $ do
            def <- initDeflate 8 $ WindowBits 31
            gziped <- feedDeflate def license >>= runPopper id
            gziped' <- runPopper gziped $ finishDeflate def
            let raw' = L.fromChunks [license]
            raw' `shouldBe` Gzip.decompress (L.fromChunks $ gziped' [])

        it "single inflate" $ do
            gziped <- S.readFile "LICENSE.gz"
            inf <- initInflate $ WindowBits 31
            popper <- feedInflate inf gziped
            ungziped <- runPopper id popper
            final <- finishInflate inf
            license `shouldBe` (S.concat $ ungziped [final])

        it "multi deflate" $ do
            let go' inf front bs = feedDeflate inf bs >>= runPopper front
            def <- initDeflate 5 $ WindowBits 31
            gziped <- foldM (go' def) id $ map S.singleton $ S.unpack license
            gziped' <- runPopper gziped $ finishDeflate def
            let raw' = L.fromChunks [license]
            raw' `shouldBe` (Gzip.decompress $ L.fromChunks $ gziped' [])

        it "multi inflate" $ do
            let go' inf front bs = feedInflate inf bs >>= runPopper front
            gziped <- S.readFile "LICENSE.gz"
            let gziped' = map S.singleton $ S.unpack gziped
            inf <- initInflate $ WindowBits 31
            ungziped' <- foldM (go' inf) id gziped'
            final <- finishInflate inf
            license `shouldBe` (S.concat $ ungziped' [final])

    describe "lbs zlib" $ do
        prop "inflate" $ \lbs -> unsafePerformIO $ do
            let glbs = compress lbs
                go' inf front bs = feedInflate inf bs >>= runPopper front
            inf <- initInflate defaultWindowBits
            inflated <- foldM (go' inf) id $ L.toChunks glbs
            final <- finishInflate inf
            return $ lbs == L.fromChunks (inflated [final])
        prop "deflate" $ \lbs -> unsafePerformIO $ do
            let go' inf front bs = feedDeflate inf bs >>= runPopper front
            def <- initDeflate 7 defaultWindowBits
            deflated <- foldM (go' def) id $ L.toChunks lbs
            deflated' <- runPopper deflated $ finishDeflate def
            return $ lbs == decompress (L.fromChunks (deflated' []))
{-
    describe "flushing" $ do
        let helper wb = do
                let bss0 = replicate 5000 "abc"
                def <- initDeflate 9 wb
                inf <- initInflate wb

                let
                  callback :: String -> [S8.ByteString] -> IO PopperRes -> IO ()
                  callback name expected pop = do
                        bssDeflated <- runPopper id pop
                        bsInflated <- -- fmap (S.concat . ($ mempty)) $
                          (forM bssDeflated $ \bs -> do
                            x <- fmap ($ mempty) . runPopper id =<< feedInflate inf bs
                            y <- flushInflate inf
                            return $ x ++ [y] ) (return id)
                        if bsInflated == expected
                            then return ()
                            else error $ "callback " ++ name ++ ", got: " ++ show bsInflated ++ ", expected: " ++ show expected

                forM_ (zip [1..] bss0) $ \(i, bs) -> do
                    feedDeflate def bs >>= callback ("loop" ++ show (i :: Int)) ""
                    callback ("loop" ++ show (i :: Int)) bs $ flushDeflate def
                callback "finish" "" $ finishDeflate def
        it "zlib" $ helper defaultWindowBits
        it "gzip" $ helper $ WindowBits 31
-}
    describe "large raw #9" $ do
        let size = fromIntegral $ LI.defaultChunkSize * 4 + 1
            input = L.replicate size 10
        it "compressing" $ do
            output <- fmap Raw.decompress $ compressRaw input
            L.all (== 10) output `shouldBe` True
            L.length output `shouldBe` L.length input
        it "decompressing" $ do
            output <- decompressRaw $ Raw.compress input
            L.all (== 10) output `shouldBe` True
            L.length output `shouldBe` L.length input

rawWindowBits :: WindowBits
rawWindowBits = WindowBits (-15)

decompressRaw :: L.ByteString -> IO L.ByteString
decompressRaw gziped = do
    inf <- initInflate rawWindowBits
    ungziped <- foldM (go' inf) id $ L.toChunks gziped
    final <- finishInflate inf
    return $ L.fromChunks $ ungziped [final]
  where
    go' inf front bs = feedInflate inf bs >>= runPopper front


compressRaw :: L.ByteString -> IO L.ByteString
compressRaw raw = do
    def <- initDeflate 1 rawWindowBits
    gziped <- foldM (go' def) id $ L.toChunks raw
    gziped' <- runPopper gziped $ finishDeflate def
    return $ L.fromChunks $ gziped' []
  where
    go' def front bs = feedDeflate def bs >>= runPopper front
