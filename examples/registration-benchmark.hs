--
-- Copyright (C) 2013 Parallel Scientific Labs, LLC. All rights reserved.
--
-- This is a benchmark comparing copying time and RMA buffer registration time.
--

{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Concurrent    ( forkIO, newEmptyMVar, takeMVar, putMVar )
import Control.Exception     ( evaluate )
import Control.Monad         ( replicateM_, void, forM_, when, replicateM, forM )
import qualified Data.ByteString as B  ( copy, replicate )
import Data.Time             ( getCurrentTime, diffUTCTime )
import Data.Word             ( Word8 )
import Foreign.C.Types       ( CSize(..), CInt(..) )
import Foreign.Marshal.Alloc ( mallocBytes, free, alloca )
import Foreign.Ptr           ( Ptr )
import Foreign.Storable      ( pokeByteOff, peek )
import Network.CCI           ( withCCI, withPollingEndpoint, rmaRegister
                             , rmaDeregister, RMA_MODE(..)
                             )
import Numeric               ( fromRat )
import Text.Printf           ( printf )
import System.Environment    ( getArgs )

foreign import ccall unsafe posix_memalign :: Ptr (Ptr a) -> CSize -> CSize -> IO CInt

allocAligned :: Int -> Int -> IO (Ptr a)
allocAligned align size = alloca $ \ptr ->
     do ret <- posix_memalign ptr (fromIntegral align) (fromIntegral size)
        when (ret /= 0) $ error $ "allocAligned: " ++ show ret
        peek ptr

main :: IO ()
main = do
    [szLogArg] <- getArgs
    let sz = 2 ^ (read szLogArg :: Int)

        iters :: Int
        iters = 100

        measureAndReport label action = do
          start <- getCurrentTime
          res <- action
          end <- getCurrentTime
          let t = toRational (diffUTCTime end start) * 1000000 / toRational iters
          printf (label ++ " time: %8.2f us\n") (fromRat t :: Double) :: IO ()
          return res

    printf "Benchmarking buffers of %d bytes ...\n" sz :: IO ()

    -- measure registration time with posix_memalign
    do
      withCCI $ withPollingEndpoint Nothing $ \ep -> do

        cbufs <- replicateM iters $ allocAligned 8 sz
        hs <- measureAndReport "registration" $
                forM cbufs $ \cbuf -> rmaRegister ep (cbuf,sz) RMA_READ_WRITE
        forM_ hs $ rmaDeregister ep
        forM_ cbufs free

    -- measure registration time when reusing addresses
    do
      withCCI $ withPollingEndpoint Nothing $ \ep -> do

        cbuf <- allocAligned 8 sz
        measureAndReport "registration+deregistration reusing address" $
          replicateM_ iters $
            rmaRegister ep (cbuf,sz) RMA_READ_WRITE >>= rmaDeregister ep
        free cbuf

    -- measure deregistration time
    do
      withCCI $ withPollingEndpoint Nothing $ \ep -> do

        cbufs <- replicateM iters $ allocAligned 8 sz
        hs <- forM cbufs $ \cbuf -> rmaRegister ep (cbuf,sz) RMA_READ_WRITE
        measureAndReport "deregistration" $
          forM_ hs $ rmaDeregister ep
        forM_ cbufs free

    -- measure copying time
    do
      buf <- evaluate $ B.replicate sz 0
      measureAndReport "copying" $
        let go _ 0 = return ()
            go x n = evaluate (B.copy x) >> go x (n-1)
         in go buf iters

    -- measure allocation time
    do
      let idxs = takeWhile (<sz) [0,4096..]
      measureAndReport "allocation+free" $
        replicateM_ iters $ do
          buf <- mallocBytes sz
          forM_ idxs $ \i -> pokeByteOff buf i (0 :: Word8)
          free buf

    -- measure mvar time
    do
      mv0 <- newEmptyMVar
      mv1 <- newEmptyMVar
      void $ forkIO $ replicateM_ iters $ takeMVar mv0 >> putMVar mv1 ()
      measureAndReport "mvar" $
        replicateM_ iters $ do
          putMVar mv0 ()
          takeMVar mv1
