--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

{-# LANGUAGE PatternGuards #-}
module TestGen
    ( testProp, TestConfig(..), defaultTestConfig, runCommands, Response(..)
    , Command(..), TestError, testCommands, ProcCommand, generateCTest
    ) where

import Control.Exception   ( catch, finally, IOException, evaluate, SomeException )
import Control.Monad       ( unless, void, forM_, replicateM, when, liftM, foldM, forM )
import Control.Monad.State ( StateT(..), MonadState(..),modify, lift, State, runState )
import qualified Data.ByteString.Char8 as B ( pack )
import Data.Function   ( on )
import Data.List       ( sort, nub, sortBy, groupBy )
import Foreign.Ptr     ( WordPtr )
import Prelude hiding  ( catch )
import System.FilePath ( (</>) )
import System.Environment ( getEnvironment )
import System.IO       ( Handle, hGetLine, hPrint, hFlush, hWaitForInput, openBinaryFile, IOMode(..) )
import System.Process  ( waitForProcess, terminateProcess, ProcessHandle
                       , CreateProcess(..), createProcess, StdStream(..), CmdSpec(..) 
                       )
import System.Random   ( Random(..), StdGen, mkStdGen )
import Text.PrettyPrint (render,empty,vcat,text,char,($$),nest,(<>),hcat)

import Commands        ( Command(..), Response(..)  )

testFolder :: FilePath
testFolder = "dist" </> "build" </> "test-Worker"

workerPath :: FilePath
workerPath = testFolder </> "test-Worker"


-- | Parameters for running tests
data TestConfig = TestConfig 
    { nProcesses :: Int -- ^ Number of processes in tests
    , nSends     :: Int -- ^ Number of sends in each interaction
    , nTries     :: Int -- ^ Number of tests to run 
    , nErrors    :: Int -- ^ Number of errors to collect before stopping
    }

defaultTestConfig :: TestConfig
defaultTestConfig = TestConfig
    { nProcesses = 2
    , nSends     = 1
    , nTries     = 100
    , nErrors    = 2
    }

type TestError = ([ProcCommand],[[Response]],String)

-- | Tests a property with a custom generated commands.
-- The property takes the issued commands, the responses that each process provided
-- and must answer if they are correct.
--
-- Several command sequences are generated. Sequences that make the property fail are
-- yielded as part of the result.
testProp :: TestConfig -> String -> ([ProcCommand] -> [[Response]] -> Bool) -> IO [TestError]
testProp c propName f = go (mkStdGen 0) [] (nErrors c) (nTries c)
  where
    go _g errors _ 0 = return errors
    go _g errors 0 _ = return errors
    go g errors errCount tryCount = do
      (tr,g') <- genCommands c g
      let tr' = map snd tr
      me <- testCommands propName tr' (nProcesses c) f
      case me of
        Just err -> do err' <- shrink propName tr err (nProcesses c) f
                       go g' (err':errors) (errCount-1) (tryCount-1)
        Nothing  -> go g' errors errCount (tryCount-1)

-- | Runs a sequence of commands and verifies that the given predicate holds on the results.
testCommands :: String -> [ProcCommand] -> Int -> ([ProcCommand] -> [[Response]] -> Bool) -> IO (Maybe TestError)
testCommands propName t nProc f = do
      r <- (fmap (Right . snd)$ runProcessM nProc$ runProcs t
                ) `catch` \e -> return$ Left (t,[],show (e :: IOException))
      case r of
        Left err -> return$ Just err
        Right rss -> if f t rss then return Nothing 
                       else return$ Just (t,rss,"failed prop: "++propName)


-- | Provides the possible ways to reduce a command sequence.
shrinkCommands :: Interaction -> [Interaction]
shrinkCommands tr = filter (not.null) [ filter ((i/=) . fst) tr  |  i<-nub (map fst tr) ]

-- | Shrinks a command sequence as much as possible while preserving a faulty behavior
-- with respect to the provided predicate.
shrink :: String -> Interaction -> TestError -> Int -> ([ProcCommand] -> [[Response]] -> Bool) -> IO TestError
shrink propName tr err nProc f = go (shrinkCommands tr)
  where
    go []       = return err 
    go (tr':trs) = do
        me <- testCommands propName (map snd tr') nProc f 
        case me of
          Just err' -> shrink propName tr' err' nProc f
          Nothing   -> go trs


runCommands :: [ProcCommand] -> IO [[Response]]
runCommands t = fmap snd$ runProcessM (1 + maximum (concatMap fst t))$ runProcs t


generateCTest :: [ProcCommand] -> String
generateCTest cmds = let procCmds = groupBy ((==) `on` fst)
                                    $ sortBy (compare `on` fst)
                                    $ concatMap (\(ps,c) -> map (flip (,) c) ps) cmds
                      in render$ includes $$ vcat (map wrapProcCmds procCmds) $$ driver cmds $$ genMain cmds
  where
    cciStatement (ConnectTo _ pd c _) = text "" $$ text ("connect(p,"++show c++",test_uri["++show pd++"]);")
    cciStatement (WaitConnection c) = text "" $$ text ("cci_connection_t* c"++show c++" = wait_connection(p,"++show c++");")
    cciStatement (Send c si msg) = text "" $$ text ("send(p,c"++show c++","++show si++","++show msg++");")
    cciStatement (Disconnect c) = text "" $$ text ("disconnect(p,c"++show c++");")
    cciStatement (WaitSendCompletion _ _) = text "" $$ text "poll_event(p);"
    cciStatement (WaitRecv _ _) = text "" $$ text "poll_event(p);"
    cciStatement (Accept _) = empty
    cciStatement cmd = text "" $$ text ("unknown command: " ++ show cmd ++";")

    wrapProcCmds cs@((i,_):_) = 
             text ("void process"++show i++"(proc_t* p) {")
             $$ nest 4 (vcat (map (cciStatement . snd) cs) 
                        $$ text ""
                        $$ text "finalize(p);"
                       )
             $$ char '}'
             $$ text ""
    wrapProcCmds _ = error "TestGen.wrapProcCmds"

    driver cs =
             text "void drive(proc_t** p) {"
             $$ nest 4 (text "char buf[100];")
             $$ nest 4 (vcat$ map readWrites cs)
             $$ char '}'
             $$ text ""

    readWrites (_,Accept _) = empty
    readWrites (ps,_) = vcat$ text "" : map (\i->text$ "write_msg(p["++show i++"],\"\");") ps 
                              ++ map (\i->text$ "read_msg(p["++show i++"],buf);") ps

    includes = vcat$ map text
        [ "#include <stdio.h>"
        , "#include <string.h>"
        , "#include <strings.h>"
        , "#include <stdlib.h>"
        , "#include <unistd.h>"
        , "#include <inttypes.h>"
        , "#include <assert.h>"
        , "#include <sys/time.h>"
        , "#include <time.h>"
        , "#include \"cci.h\""
        , "#include \"testlib.h\""
        , ""
        ]

    genMain cs =
         let maxProcIndex = maximum$ concatMap fst cs
             ps = sort$ nub$ concatMap fst cs
          in text "int main(int argc, char *argv[]) {"
             $$ (nest 4$ vcat$
                   [ text "proc_t* p[100];"
                   , text "memset(p,0,100*sizeof(proc_t*));"
                   , text ""
                   , hcat (map vcat$ map1 (mapFirst (nest 7))$ map (genSpawn (maxProcIndex+1)) ps)
                       <> vcat 
                          [ nest 7$ text "{"
                          , nest 4$ vcat
                              [ text ("write_uris(p,"++show (maxProcIndex+1)++");")
                              , text "drive(p);"
                              , text "wait();"
                              ]
                          , char '}' 
                          , text "return 0;"
                          ]
                   ]
                )
             $$ char '}'

    genSpawn n i = [ text$ "if (!spawn(&p["++show i++"],"++show i++")) {"
                   , nest 4 (
                       text ("read_uris(p["++show i++"],"++show n++");")
                       $$ text ("process"++show i++"(p["++show i++"]);")
                     )
                   , text "} else "
                   ]

    map1 _ [] = []
    map1 f (x:xs) = x : map f xs

    mapFirst _ [] = []
    mapFirst f (x:xs) = f x : xs

-- Command generation

-- | Takes the amount of processes, and generates commands for an interaction among them.
genCommands :: TestConfig -> StdGen -> IO (Interaction,StdGen)
genCommands c g = return$ runCommandGenDefault g$ 
               mapM (uncurry (genInteraction c)) (zip (nProcesses c-1:[0..]) [0..nProcesses c-1])
                 >>= foldM mergeI []

-- | An interaction is a list of commands for a given process.
-- Because an interaction might be the result of merging simpler
-- interactions, each command is attached with an identifier of the
-- simplest interaction that contained it. 
--
-- @[(interaction_id,(destinatary_process_ids,command))]@
--
-- The purpose of the interaction identifier is to allow to shrink
-- a failing test case by removing the interactions that do not affect 
-- the bug reproduction (see function 'shrink').
type Interaction = [(Int,ProcCommand)]

type ProcCommand = ([Int],Command)


runCommandGen :: CommandGenState -> CommandGen a -> (a,CommandGenState)
runCommandGen = flip runState 

runCommandGenDefault :: StdGen -> CommandGen a -> (a,StdGen)
runCommandGenDefault g = (\(a,s)->(a,rG s)) .
    runCommandGen CommandGenState 
        { connG = 0
        , sendG = 0
        , interactionG = 0
        , rG = g
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
genInteraction :: TestConfig -> Int -> Int -> CommandGen Interaction 
genInteraction c p0 p1 = do
    i <- generateInterationId
    mt <- getRandomTimeout
    cid <- generateConnectionId
    sends <- genSends cid p0 p1 0 0 (nSends c)
    return$ (i,([p1],Accept cid)):
       (zip (repeat i) ( ([p0],ConnectTo "" p1 cid mt) : ([p0,p1],WaitConnection cid) : sends
                         )) -- ++ [([p0],WaitEvents (nSends c+1)),([p1],WaitEvents (nSends c+2))] ))
  where
    getRandomTimeout = do
        b <- getRandom
        if b then return Nothing
          else return Nothing -- fmap (Just . (+6*1000000))$ getRandom

    genSends :: WordPtr -> Int -> Int -> Int -> Int -> Int -> CommandGen [ProcCommand]
    genSends cid p0 p1 w0 w1 0 = return$ 
               [ ([p0],WaitSendCompletion cid (fromIntegral sid)) | sid <- [w0,w0-1..1] ]
               ++ [ ([p1],WaitRecv cid (fromIntegral rid)) | rid <- [w1,w1-1..1]  ]
               ++ [ ([p0],Disconnect cid) ]
    genSends cid p0 p1 w0 w1 mid = do
        insertWaits0 <- getRandom 
        insertWaits1 <- getRandom 
        let (waits0,w0') = if insertWaits0 
                             then ([ ([p0],WaitSendCompletion cid (fromIntegral sid)) | sid <- [w0+mid,w0+mid-1..mid+1] ] ,0) 
                             else ([],w0)
            (waits1,w1') = if insertWaits1 
                             then ([ ([p1],WaitRecv cid (fromIntegral rid)) | rid <- [w1+mid,w1+mid-1..mid+1]  ] ,0) 
                             else ([],w1)
        rest <- genSends cid p0 p1 (w0'+1) (w1'+1) (mid-1)
        return$ waits0 ++ waits1 ++ ([p0],Send cid (fromIntegral mid) (B.pack$ show mid)) : rest



-- A monad for processes

type ProcessM a = StateT ([Process],[[Response]]) IO a

runProcessM :: Int -> ProcessM a -> IO (a,[[Response]])
runProcessM n m = do
    ps <- mapM launchWorker [0..n-1]
    (fmap (\(a,(_,rs))-> (a,rs))$ runStateT m (ps,map (const []) ps))
      `finally` (do forM_ ps (\p -> terminateProcess (ph p))
                    forM_ ps (\p -> waitForProcess (ph p)))

runProcs :: [ProcCommand] -> ProcessM ()
runProcs tr = do
   forM_ tr$ \(pids,c) -> do
                ps <- forM pids$ \pid -> do
                        p <- getProc pid
                        sendCommand' c p
                        return (p,pid)
                forM_ ps$ uncurry readResponses 

   fmap fst get >>= \ps -> forM_ [0..length ps-1]$ sendCommand Quit


-- | Process communication

data Process = Process 
    { h_in :: Handle
    , h_out :: Handle
    , ph :: ProcessHandle
    , uri :: String
    }

launchWorker :: Int -> IO Process
launchWorker pid = do
    -- (hin,hout,herr,phandle) <- runInteractiveProcess workerPath [] Nothing (Just [("CCI_CONFIG","cci.ini"),("LD_LIBRARY_PATH","cci/built/lib")])
    herr <- openBinaryFile ("worker-stderr"++show pid++".txt") WriteMode
    env <- getEnvironment
    (Just hin,Just hout,_herr,phandle) <- createProcess CreateProcess 
                                  { cmdspec = RawCommand workerPath []
                                  , cwd = Nothing
                                  , env = Just$ [("CCI_CONFIG","cci.ini")]++env
                                  , std_in = CreatePipe
                                  , std_out = CreatePipe
                                  , std_err = UseHandle herr
                                  , close_fds = False
                                  }

    -- (hin,hout,herr,phandle) <- runInteractiveCommand$ "CCI_CONFIG=cci.ini "++workerPath++" 2> worker-stderr"++show pid++".txt"
    -- void$ forkIO$ hGetContents herr >>= writeFile ("worker-stderr"++show pid++".txt") >> putStrLn ("wrote "++show pid)
    puri <- hGetLine hout
    return Process 
        { h_in = hin
        , h_out = hout
        , ph = phandle
        , uri = puri
        }

sendCommand :: Command -> Int -> ProcessM ()
sendCommand c i = do
    p <- getProc i
    sendCommand' c p
    readResponses p i

sendCommand' :: Command -> Process -> ProcessM ()
sendCommand' c p = do
    c' <- case c of
            ConnectTo _ pid cid mt -> 
               getProc pid >>= \pd -> return$ ConnectTo (uri pd) pid cid mt
            _ -> return c
    lift$ hPrint (h_in p) c'
    lift$ hFlush (h_in p)


readResponses :: Process -> Int -> ProcessM ()
readResponses p i = void$ loopWhileM (/=Idle)$ readResponse p i

readResponse :: Process -> Int -> ProcessM Response
readResponse p i = do
        someInput <- lift$ hWaitForInput (h_out p) 2000
        lift$ when (not someInput)$ ioError$ userError$ "process "++show i++" blocked" 
        line <- lift$ hGetLine (h_out p)
        r <- lift$ evaluate (read line) `catch` 
                  (\e -> ioError$ userError$ "failed reading response: "++show line++" with "++show (e :: SomeException))
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

