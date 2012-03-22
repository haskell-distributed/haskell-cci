--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

{-# LANGUAGE PatternGuards #-}

import Control.Exception   ( catch, finally, IOException )
import Control.Monad       ( unless, void, forM_, replicateM, when, liftM, foldM )
import Control.Monad.State ( StateT(..), MonadState(..),modify, lift, State, runState )
import qualified Data.ByteString.Char8 as B ( pack )
import Data.List       ( sort )
import Debug.Trace     ( trace )
import Foreign.Ptr     ( WordPtr )
import Prelude hiding  ( catch )
import System.FilePath ( (</>) )
import System.Exit     ( exitFailure,exitWith,ExitCode(..))
import System.IO       ( Handle, hGetLine, hPrint, hFlush, hReady, hIsClosed, hIsOpen, hWaitForInput )
import System.Process  ( rawSystem,runInteractiveProcess,terminateProcess,readProcess, ProcessHandle, runInteractiveCommand)
import System.Random   ( RandomGen(..), Random(..), StdGen, mkStdGen )

import Commands        ( Command(..), Response(..)  )

testFolder :: FilePath
testFolder = "dist" </> "build" </> "test-Worker"

workerPath :: FilePath
workerPath = testFolder </> "test-Worker"

nProc :: Int
nProc = 2

main :: IO ()
main = testProp$ \t rs -> trace (show t ++ show rs) True

-- | Tests a property with a custom generated trace of commands.
-- The property takes the issued commands, the responses that each process provided
-- and must answer if they are correct.
testProp :: ([(Int,Command)] -> [[Response]] -> Bool) -> IO ()
testProp f = do
   tr <- genTrace nProc
   print tr
   let tr' = map snd tr
   (do (_,rss) <- runProcessM nProc$ runProcs tr' 
       when (not (f tr' rss)) exitFailure 
    ) `catch` \e -> do putStrLn "Trace failed to execute: "
                       print tr'
                       putStrLn ("with exception: "++ show (e :: IOException))
                       exitFailure

runTrace :: [(Int,Command)] -> IO [[Response]]
runTrace t = fmap snd$ runProcessM (1 + maximum (map fst t))$ runProcs t

-- Trace generation

-- | Takes the amount of processes, and generates commands for an interaction among them.
genTrace :: Int -> IO Interaction
genTrace n = return$ runCommandGenDefault 0$ 
               mapM (uncurry genInteraction) [(1,0)]  -- (zip (n-1:[0..]) [0..n-1])
                 >>= foldM mergeI []

-- | An interaction is a list of commands for a given process.
-- Because an interaction might be the result of merging simpler
-- interactions, each command is attached with an identifier of the
-- simplest interaction that contained it. 
--
-- @[(interaction_id,(destinatary_process_id,command))]@
--
-- The purpose of the interaction identifier is to allow to shrink
-- a failing test case by removing the interactions that do not affect 
-- the bug reproduction.
type Interaction = [(Int,(Int,Command))]


runCommandGen :: CommandGenState -> CommandGen a -> (a,CommandGenState)
runCommandGen = flip runState 

runCommandGenDefault :: Int -> CommandGen a -> a
runCommandGenDefault i = fst .
    runCommandGen CommandGenState 
        { connG = 0
        , sendG = 0
        , interactionG = 0
        , rG = mkStdGen i
        }


modifyR :: (StdGen -> (a,StdGen)) -> CommandGen a
modifyR f = modifyCGS (\s -> let (a,g) = f (rG s) in (a,s { rG = g}) )

getRandom :: Random a => CommandGen a
getRandom = modifyR random

getRandomR :: Random a => (a,a) -> CommandGen a
getRandomR = modifyR . randomR

-- | Merges two interactions by randomly interleaving the commands. The 
-- order of the commands in each interaction is preserved.
mergeI :: Interaction -> Interaction -> CommandGen Interaction
mergeI xs [] = return xs
mergeI [] ys = return ys
mergeI i0 i1 = if l0<=l1 then mergeI' i0 l0 i1 l1 else mergeI' i1 l1 i0 l0
  where
    l0 = length i0
    l1 = length i1

mergeI' :: Interaction -> Int -> Interaction -> Int -> CommandGen Interaction
mergeI' i0 l0 i1 l1 = do
    iss <- replicateM l0$ getRandomR (0,l0+l1)
    let (i:is) = sort iss
    return$ insertI 0 i is i0 i1
  where
    -- | Given indexes (i:is) in [0..l0+l1-1] inserts elements of i0 among elements of i1
    -- so the positions of the i0 elements in the result match those of the indexes (i:is)
    insertI p i is i0 i1 
        | p<i, (ir1:irs1) <- i1            = ir1 : insertI (p+1) i is i0 irs1
        | (i':is') <- is, (ir0:irs0) <- i0 = ir0 : insertI (p+1) i' is' irs0 i1
        | otherwise                        = i0 ++ i1

{-
mergeI xss@(x:xs) yss@(y:ys) = getRandom >>= \b -> 
    if b then liftM (x:)$ mergeI xs yss
      else liftM (y:)$ mergeI xss ys
-}


-- | There are quite a few identifiers which are used to organize the data.
--
-- This datatype stores generators for each kind of identifier.
data CommandGenState = CommandGenState
    { connG :: WordPtr
    , sendG :: WordPtr
    , interactionG :: Int
    , rG :: StdGen
    }

type CommandGen a = State CommandGenState a

modifyCGS :: (CommandGenState -> (a,CommandGenState)) -> CommandGen a
modifyCGS f = liftM f get >>= \(a,g) -> put g >> return a

generateInterationId :: CommandGen Int
generateInterationId = modifyCGS (\g -> (interactionG g,g { interactionG = interactionG g+1}))

generateConnectionId :: CommandGen WordPtr
generateConnectionId = modifyCGS (\g -> (connG g,g { connG = connG g+1}))


-- | Takes two processes identifiers and generates an interaction between them.
--
-- Right now the interactions consist on stablishing an initial connection and 
-- then having the first process send messages to second one.
genInteraction :: Int -> Int -> CommandGen Interaction 
genInteraction p0 p1 = do
    let numSends = 1
    i <- generateInterationId
    mt <- getRandomTimeout
    cid <- generateConnectionId
    sends <- genSends cid p0 p1 0 0 numSends
    return$ (i,(p1,Accept cid)):
       (zip (repeat i) ( (p0,ConnectTo "" p1 cid mt) : (p1,WaitEvent) : (p1,WaitEvent) : (p0,WaitEvent) : sends))
  where
    getRandomTimeout = do
        b <- getRandom
        if b then return Nothing
          else fmap (Just . (+6*1000000))$ getRandom

    genSends :: WordPtr -> Int -> Int -> Int -> Int -> Int -> CommandGen [(Int,Command)]
    genSends cid p0 p1 w0 w1 0 = return$ replicate w0 (p0,WaitEvent) ++ replicate w1 (p1,WaitEvent) ++[ (p0,Disconnect cid) ]
    genSends cid p0 p1 w0 w1 i = do
        insertWaits0 <- getRandom 
        insertWaits1 <- getRandom 
        let (waits0,w0') = if insertWaits0 then (replicate w0 (p0,WaitEvent),0) else ([],w0)
            (waits1,w1') = if insertWaits1 then (replicate w1 (p1,WaitEvent),0) else ([],w1)
        rest <- genSends cid p0 p1 (w0'+1) (w1'+1) (i-1)
        return$ waits0 ++ waits1 ++ (p0,Send 0 (fromIntegral i) (B.pack$ show i)) : rest



-- A monad for processes

type ProcessM a = StateT ([Process],[[Response]]) IO a

runProcessM :: Int -> ProcessM a -> IO (a,[[Response]])
runProcessM n m = do
    ps <- mapM launchWorker [0..n-1]
    (fmap (\(a,(_,rs))-> (a,rs))$ runStateT m (ps,map (const []) ps))
      `finally` forM_ ps (\p -> terminateProcess (ph p))

runProcs :: [(Int,Command)] -> ProcessM ()
runProcs tr = do
   forM_ tr$ \(i,c) -> sendCommand c i
   fmap fst get >>= \ps -> forM_ [0..length ps-1]$ sendCommand Quit

{-
runProcs :: [(Int,Command)] -> ProcessM ()
runProcs tr = do
   forM_ tr$ \(i,c) -> sendCommand c i
   fmap fst get >>= \ps -> do
      forM_ [0..length ps-1]$ sendCommand Quit
      readResps (zip [0..] ps)
  where
    readResps [] = return ()
    readResps ips = do
      forM_ ips$ \(i,p) -> readResponses p i
      ips' <- lift$ filterM (hIsOpen . h_out . snd) ips
      readResps ips'

 - -}

-- | Process communication

data Process = Process 
    { h_in :: Handle
    , h_out :: Handle
    , h_err :: Handle
    , ph :: ProcessHandle
    , uri :: String
    }

launchWorker :: Int -> IO Process
launchWorker pid = do
    -- (hin,hout,herr,phandle) <- runInteractiveProcess workerPath [] Nothing (Just [("CCI_CONFIG","cci.ini")])
    (hin,hout,herr,phandle) <- runInteractiveCommand$ "CCI_CONFIG=cci.ini "++workerPath++" 2> worker-stderr"++show pid++".txt"
    puri <- hGetLine hout
    return Process 
        { h_in = hin
        , h_out = hout
        , h_err = herr
        , ph = phandle
        , uri = puri
        }

sendCommand :: Command -> Int -> ProcessM ()
sendCommand c i = do
    p <- getProc i
    c' <- case c of
            ConnectTo _ pid cid mt -> 
               getProc pid >>= \pd -> return$ ConnectTo (uri pd) pid cid mt
            _ -> return c
    lift$ hPrint (h_in p) c'
    lift$ hFlush (h_in p)
    readResponses p i


readResponses :: Process -> Int -> ProcessM ()
readResponses p i = void$ loopWhileM (/=Idle)$ readResponse p i

readResponse :: Process -> Int -> ProcessM Response
readResponse p i = do
        someInput <- lift$ hWaitForInput (h_out p) 2000
        lift$ when (not someInput)$ ioError$ userError$ "process "++show i++" blocked" 
        r <- lift$ fmap read $ hGetLine (h_out p)
        unless (r==Idle)$ addResponse r i
        return r


addResponse :: Response -> Int -> ProcessM ()
addResponse resp n = modify (\(ps,rs) -> (ps,insertR n resp rs))
  where
    insertR 0 r (rs:rss) = (r:rs) : rss
    insertR i r (rs:rss) = rs : insertR (i-1) r rss
    insertR i _ []       = error$ "Process "++show i++" does not exist."


getProc :: Int -> ProcessM Process
getProc i = get >>= return . (!!i) . fst

-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: Monad m => (a -> Bool) -> m a -> m a
loopWhileM p io = io >>= \a -> if p a then loopWhileM p io else return a

