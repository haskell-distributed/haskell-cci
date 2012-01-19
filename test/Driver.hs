--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

{-# LANGUAGE PatternGuards #-}

import Control.Monad       ( unless, void, forM_, replicateM, when, liftM, foldM )
import Control.Monad.State ( StateT(..), MonadState(..),modify, lift, State, runState )
import qualified Data.ByteString.Char8 as B ( pack )
import Data.List       ( sort )
import Foreign.Ptr     ( WordPtr )
import System.FilePath ( (</>) )
import System.Exit     ( exitFailure,exitWith,ExitCode(..))
import System.IO       ( Handle, hGetLine, hPrint )
import System.Process  ( rawSystem,runInteractiveProcess,terminateProcess,readProcess, ProcessHandle)
import System.Random   ( RandomGen(..), Random(..), StdGen, mkStdGen )

import Commands        ( Command(..), Response(..)  )

testFolder :: FilePath
testFolder = "dist" </> "build" </> "test-cci"

testSrcFolder :: FilePath
testSrcFolder = "test"

workerPath :: FilePath
workerPath = testFolder </> "Worker"

nProc :: Int
nProc = 4

main :: IO ()
main = do
    putStrLn "compiling worker program ..."
    check$ rawSystem "ghc" [ "--make","-lcci","-idist"</>"build","-i.","-i"++testSrcFolder
                           , "-odir",testFolder,"-hidir",testFolder,testSrcFolder</>"Worker.hs"
                           ]
    testProp undefined
    
  where
    check io = io >>= \c -> case c of { ExitSuccess -> return (); _ -> exitWith c }


testProp :: ([(Int,Command)] -> [[Response]] -> Bool) -> IO ()
testProp f = do
   tr <- genTrace nProc
   let tr' = map snd tr
   (_,rss) <- runProcessM nProc$ runProcs tr'
   when (not (f tr' rss)) exitFailure 



-- Trace generation

genTrace :: Int -> IO [(Int,(Int,Command))]
genTrace n = return$ runCommandGenDefault 0$ 
               mapM (uncurry genInteraction) (zip (n-1:[0..]) [0..n-1])
                 >>= foldM mergeI []

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


genInteraction :: Int -> Int -> CommandGen Interaction 
genInteraction p0 p1 = do
    let numSends = 5
    i <- generateInterationId
    mt <- getRandomTimeout
    cid <- generateConnectionId
    sends <- genSends cid 0 numSends
    mergeI ((i,(p1,Accept cid)): zip (repeat i) (zip (repeat p0)$ ConnectTo "" cid mt : sends))
           (zip (repeat i)$ zip (repeat p1)$ replicate (numSends+1) WaitEvent)
  where
    getRandomTimeout = do
        b <- getRandom
        if b then return Nothing
          else fmap (Just . (+6*1000000))$ getRandom

    genSends :: WordPtr -> Int -> Int -> CommandGen [Command]
    genSends cid w 0 = return$ replicate w WaitEvent ++[ Disconnect cid ]
    genSends cid w i = do
        insertWaits <- getRandom 
        let (waits,w') = if insertWaits then (replicate w WaitEvent,0) else ([],w)
        rest <- genSends cid (w'+1) (i-1)
        return$ waits ++ Send 0 (fromIntegral i) (B.pack$ show i) : rest



-- A monad for processes

type ProcessM a = StateT ([Process],[[Response]]) IO a

runProcessM :: Int -> ProcessM a -> IO (a,[[Response]])
runProcessM n m = do
    ps <- replicateM n launchWorker 
    fmap (\(a,(_,rs))-> (a,rs))$ runStateT m (ps,map (const []) ps)

runProcs :: [(Int,Command)] -> ProcessM ()
runProcs tr = forM_ tr$ \(i,c) -> sendCommand c i


-- | Process communication

data Process = Process 
    { h_in :: Handle
    , h_out :: Handle
    , h_err :: Handle
    , ph :: ProcessHandle
    , uri :: String
    }

launchWorker :: IO Process
launchWorker = do
    (hin,hout,herr,phandle) <- runInteractiveProcess workerPath [] Nothing (Just [("CCI_CONFIG","cci.ini")])
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
    lift$ hPrint (h_in p) c
    void$ loopWhileM (/=Idle)$ do
        r <- lift$ fmap (read :: String -> Response)$ hGetLine (h_out p)
        unless (r==Idle)$ addResponse r i
        return r

readResponses :: Int -> ProcessM ()
readResponses i = do
    p <- getProc i
    void$ loopWhileM (/=Idle)$ do
        r <- lift$ fmap  (read :: String -> Response)$ hGetLine (h_out p)
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

