{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..),  (===))
import Data.Foldable (fold)
import Data.List
import Codec.Zlib
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Control.Monad (foldM, forM_)
import System.IO.Unsafe (unsafePerformIO)
import Codec.Compression.Zlib hiding (WindowBits, defaultWindowBits)
import qualified Codec.Compression.GZip as Gzip
import qualified Codec.Compression.Zlib.Raw as Raw

gzipWindowBits = WindowBits 31

inflateChunk :: Inflate -> ([S8.ByteString] -> c) -> S8.ByteString -> IO ([S8.ByteString] -> c)
inflateChunk inf front bs = feedInflate inf bs >>= runPopper front

-- inflateChunkCopy :: Inflate -> ([S8.ByteString] -> c) -> S8.ByteString -> IO (Inflate, [S8.ByteString] -> c)
-- inflateChunkCopy inf front bs = do
--   (inf', pop) <- feedInflateCopy inf bs
--   k <- runPopper front pop
--   return (inf', k)

deflateChunk :: Deflate -> ([S8.ByteString] -> c) -> S8.ByteString -> IO ([S8.ByteString] -> c)
deflateChunk inf front bs = feedDeflate inf bs >>= runPopper front

runPopper :: ([S8.ByteString] -> c) -> Popper -> IO ([S8.ByteString] -> c)
runPopper front x = do
    y <- x
    case y of
      PRDone -> return front
      PRError _ -> return front
      PRNext z -> runPopper (front . (:) z) x

decompress' :: L.ByteString -> L.ByteString
decompress' gziped = unsafePerformIO $ do
    inf <- initInflate defaultWindowBits
    ungziped <- foldM (inflateChunk inf) id $ L.toChunks gziped
    final <- finishInflate inf
    return $ L.fromChunks $ ungziped [final]

-- decompressCopy :: L.ByteString -> L.ByteString
-- decompressCopy gziped = unsafePerformIO $ do
--   inf <- initInflate defaultWindowBits
--   (inf', ungziped) <- foldM (uncurry inflateChunkCopy) (inf, id) $ L.toChunks gziped
--   final <- finishInflate inf'
--   return $ L.fromChunks $ ungziped [final]

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks `fmap` arbitrary
instance Arbitrary S.ByteString where
    arbitrary = S.pack `fmap` arbitrary

compress' :: L.ByteString -> L.ByteString
compress' raw = unsafePerformIO $ do
    def <- initDeflate 7 defaultWindowBits
    gziped <- foldM (deflateChunk def) id $ L.toChunks raw
    gziped' <- runPopper gziped $ finishDeflate def
    return $ L.fromChunks $ gziped' []

license :: S.ByteString
license = S8.filter (/= '\r') $ unsafePerformIO $ S.readFile "LICENSE"

exampleDict :: S.ByteString
exampleDict = "INITIALDICTIONARY"

deflateWithDict :: S.ByteString -> L.ByteString -> L.ByteString
deflateWithDict dict raw = unsafePerformIO $ do
    def <- initDeflateWithDictionary 7 dict $ WindowBits 15
    compressed <- foldM (deflateChunk def) id $ L.toChunks raw
    compressed' <- runPopper compressed $ finishDeflate def
    return $ L.fromChunks $ compressed' []

inflateWithDict :: S.ByteString -> L.ByteString -> L.ByteString
inflateWithDict dict compressed = unsafePerformIO $ do
    inf <- initInflateWithDictionary (WindowBits 15) dict
    decompressed <- foldM (inflateChunk inf) id $ L.toChunks compressed
    final <- finishInflate inf
    return $ L.fromChunks $ decompressed [final]

main :: IO ()
main = hspec $ do
    -- describe "inflate/deflate" $ do
    --     prop "decompress'" $ \lbs -> lbs == decompress' (compress lbs)
    --     prop "decompressCopy" $ \lbs -> lbs == decompressCopy (compress lbs)
    --     prop "compress'" $ \lbs -> lbs == decompress (compress' lbs)

    --     prop "with dictionary" $ \bs ->
    --         bs ==
    --         (inflateWithDict exampleDict . deflateWithDict exampleDict) bs
    --     it "different dict" $ do
    --         raw <- L.readFile "LICENSE"
    --         let deflated = deflateWithDict exampleDict raw
    --             inflated = inflateWithDict (S.drop 1 exampleDict) deflated
    --         inflated `shouldSatisfy` L.null

    describe "license" $ do
        -- it "single deflate" $ do
        --     def <- initDeflate 8 $ WindowBits 31
        --     gziped <- feedDeflate def license >>= runPopper id
        --     gziped' <- runPopper gziped $ finishDeflate def
        --     let raw' = L.fromChunks [license]
        --     Gzip.decompress (L.fromChunks $ gziped' []) `shouldBe` raw'

        -- it "single inflate" $ do
        --     gziped <- S.readFile "LICENSE.gz"
        --     inf <- initInflate $ WindowBits 31
        --     ungziped <- inflateChunk inf id gziped
        --     final <- finishInflate inf
        --     (S.concat $ ungziped [final]) `shouldBe` license

        -- it "single inflate copy" $ do
        --     gziped <- S.readFile "LICENSE.gz"
        --     inf <- initInflate $ WindowBits 31
        --     (inf',ungziped) <- inflateChunkCopy inf id gziped
        --     final <- finishInflate inf'
        --     (S.concat $ ungziped [final]) `shouldBe` license

        -- it "multi deflate" $ do
        --     def <- initDeflate 5 $ WindowBits 31
        --     gziped <- foldM (deflateChunk def) id $ map S.singleton $ S.unpack license
        --     gziped' <- runPopper gziped $ finishDeflate def
        --     let raw' = L.fromChunks [license]
        --     (Gzip.decompress $ L.fromChunks $ gziped' []) `shouldBe` raw'

        it "multi inflate" $ do
            gziped <- S.readFile "LICENSE.gz"
            let gziped' = map S.singleton $ S.unpack gziped
            inf <- initInflate $ WindowBits 31
            ungziped' <- foldM (inflateChunk inf) id gziped'
            final <- finishInflate inf
            (S.concat $ ungziped' [final]) `shouldBe` license

        it "multi inflate segfault" $ do
            gziped <- S.readFile "LICENSE.gz"
            let gziped' = map S.singleton $ S.unpack gziped
            inf <- initInflate $ WindowBits 31
            ungziped' <- foldM (\k c -> do
                                   k' <- inflateChunk inf k c
                                   inflateChunk inf k' c
                               ) id gziped'
            final <- finishInflate inf
            (S.concat $ ungziped' [final]) `shouldBe` license

        it "multi inflate copy" $ do
            gziped <- S.readFile "LICENSE.gz"
            let gziped' = map S.singleton $ S.unpack gziped
            inf <- initInflate $ WindowBits 31
            (inf',ungziped') <- foldM (\(st, k) c -> do
                                          (inf', bss) <- feedInflateCopy st c
                                          return (inf', (bss ++) . k)
                                      ) (inf,id) gziped'
            final <- finishInflate inf'
            (S.concat $ ungziped' [final]) `shouldBe` license

        -- it "fkladsjfkldsjf" $ do
        --   gziped <- S.readFile "LICENSE.gz"
        --   let chunks = map S.singleton $ S.unpack gziped

        --   let inf = initInflate $ WindowBits

        it "multi inflate copy CPS" $ do
            gziped <- S.readFile "LICENSE.gz"
            let gziped' = map S.singleton $ S.unpack gziped

            let
              infk = \k -> k $! (initInflate' gzipWindowBits, id)

              infk2
              -- foldr goes in reverse order
                = foldl ( \prevk c ->
                           \k -> prevk $! \(inflate, front) ->
                             k  $! (fmap (\x rest -> x ++ rest))
                                    (  (feedInflateCopy' inflate c ))
                         )
                  infk
                  gziped'

              xxx
                = infk2 (\(inflate, front) ->
                            let !fff = finishInflate' inflate
                            in front $! [fff]
                        )

            (S.concat xxx) `shouldBe` license


    -- describe "lbs zlib" $ do
    --     prop "inflate" $ \lbs -> unsafePerformIO $ do
    --         let glbs = compress lbs
    --         inf <- initInflate defaultWindowBits
    --         inflated <- foldM (inflateChunk inf) id $ L.toChunks glbs
    --         final <- finishInflate inf
    --         return $ lbs == L.fromChunks (inflated [final])
    --     prop "deflate" $ \lbs -> unsafePerformIO $ do
    --         def <- initDeflate 7 defaultWindowBits
    --         deflated <- foldM (deflateChunk def) id $ L.toChunks lbs
    --         deflated' <- runPopper deflated $ finishDeflate def
    --         return $ lbs == decompress (L.fromChunks (deflated' []))


        prop "UNSAFE inflate copy with cps" $ \lbs ->
          let
            glbs = compress lbs
            infk = \k -> k $! (initInflate' (WindowBits 15),  id)

            infk2
              = foldl (\prevk c ->
                         \k -> prevk $! \(inflate, front) ->
                           k $! (fmap (\x rest -> x ++ rest))
                                  (  (feedInflateCopy' inflate c ))
                       )
                infk
                (L.toChunks glbs)

            xxxxx = infk2 (\(inflate, k) -> k $! [finishInflate' inflate] )
          in
            lbs === L.fromChunks xxxxx

    -- describe "flushing" $ do
    --     let helper wb = do
    --             let bss0 = replicate 5000 "abc"
    --             def <- initDeflate 9 wb
    --             inf <- initInflate wb

    --             let callback name expected pop = do
    --                     bssDeflated <- runPopper id pop
    --                     bsInflated  <- fmap (fold . fold)
    --                                    . (mapM $ \bs -> do
    --                                          x <- fmap ($ mempty) . runPopper id =<< feedInflate inf bs
    --                                          y <- flushInflate inf
    --                                          return $ x ++ [y] )
    --                                    . bssDeflated
    --                                    $ mempty

    --                     if bsInflated == expected
    --                         then return ()
    --                         else error $ "callback " ++ name ++ ", got: " ++ show bsInflated ++ ", expected: " ++ show expected

    --             forM_ (zip [1..] bss0) $ \(i, bs) -> do
    --                 feedDeflate def bs >>= callback ("loop" ++ show (i :: Int)) ""
    --                 callback ("loop" ++ show (i :: Int)) bs $ flushDeflate def
    --             callback "finish" "" $ finishDeflate def
    --     it "zlib" $ helper defaultWindowBits
    --     it "zip" $ helper $ WindowBits 31

    -- describe "large raw #9" $ do
    --     let size = fromIntegral $ LI.defaultChunkSize * 4 + 1
    --         input = L.replicate size 10
    --     it "compressing" $ do
    --         output <- fmap Raw.decompress $ compressRaw input
    --         L.all (== 10) output `shouldBe` True
    --         L.length output `shouldBe` L.length input
    --     it "decompressing" $ do
    --         output <- decompressRaw $ Raw.compress input
    --         L.all (== 10) output `shouldBe` True
    --         L.length output `shouldBe` L.length input
    --     it "decompressing" $ do
    --         output <- decompressRawCopy $ Raw.compress input
    --         L.all (== 10) output `shouldBe` True
    --         L.length output `shouldBe` L.length input

rawWindowBits :: WindowBits
rawWindowBits = WindowBits (-15)

decompressRaw :: L.ByteString -> IO L.ByteString
decompressRaw gziped = do
    inf <- initInflate rawWindowBits
    ungziped <- foldM (inflateChunk inf) id $ L.toChunks gziped
    final <- finishInflate inf
    return $ L.fromChunks $ ungziped [final]

-- decompressRawCopy :: L.ByteString -> IO L.ByteString
-- decompressRawCopy gziped = do
--     inf <- initInflate rawWindowBits
--     (inf',ungziped) <- foldM (uncurry inflateChunkCopy) (inf,id) $ L.toChunks gziped
--     final <- finishInflate inf'
--     return $ L.fromChunks $ ungziped [final]

compressRaw :: L.ByteString -> IO L.ByteString
compressRaw raw = do
    def <- initDeflate 1 rawWindowBits
    gziped <- foldM (deflateChunk def) id $ L.toChunks raw
    gziped' <- runPopper gziped $ finishDeflate def
    return $ L.fromChunks $ gziped' []
