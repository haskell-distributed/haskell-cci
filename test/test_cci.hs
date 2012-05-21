--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

import Control.Monad   ( when, forM_ )
import qualified Data.Map as M( empty, insert, lookup )
import GHC.Exts        ( groupWith )
import System.Exit     ( exitFailure )

import TestGen         ( testProp, defaultTestConfig, runCommands, Command(..), Msg(..)
                       , ProcCommand, Response(..), TestConfig(..), generateCTest )
import Network.CCI    ( Status(..) )
import System.Console.CmdArgs

main :: IO ()
main = props -- writeCTest -- singleTest -- props

props :: IO ()
props = do
    tc <- cmdArgs (TestConfig { nSends= intopt "Amount of messages sent per interaction" (nSends defaultTestConfig)
                              , nTries= intopt "Amount of command sequences to generate" (nTries defaultTestConfig)
                              , nProcesses= intopt "Amount of processes to test" (nProcesses defaultTestConfig)
                              , nMinMsgLen= intopt "Minimum size of messages to test" (nMinMsgLen defaultTestConfig)
                              , nMaxMsgLen= intopt "Maximum size of messages to test" (nMaxMsgLen defaultTestConfig)
                              , nPerProcessInteractions=
                                     intopt "Amount of interactions each process initiates during a test" (nPerProcessInteractions defaultTestConfig)
                                     &= name "nPerProcRuns" &= explicit
                              , nErrors= intopt "Amount of errors to collect before stopping testing" (nErrors defaultTestConfig)
                              }
                                &= summary "CCI test generator v0.0.1, (C) Parallel Scientific 2012"
                                &= details ["See test/README for details."]
                                &= program "test_cci"
                  )
    errs <- testProp tc$ \cmds rs ->
                [ ( "sends equal completions"
                  , length (filter (isSendCommand . snd) cmds) == length (concatMap (filter isSendCompletion) rs) 
                  )
                , ( "messages arrive sorted"
                  , matchSendRecvs (collectSends cmds) (collectRecvs rs)
                  )
                ]
    mapM_ print errs
    let cfilename :: Int -> String
        cfilename i = "t"++show i++".c"
    forM_ (zip errs [0..])$ \((s,_,_),i) -> writeFile (cfilename i)$ generateCTest s
    when (not (null errs))$ do
       putStrLn$ "Test programs have been written to " ++ show (map cfilename [0..length errs-1])
       exitFailure
  where
    isSendCompletion (SendCompletion _ _ _) = True
    isSendCompletion _ = False
    intopt :: (Default a,Data a,Show a) => String -> a -> a
    intopt s i = i &= help (s++" [default: "++show i++"]") &= opt i

isSendCommand :: Command -> Bool
isSendCommand (Send _ _ _) = True
isSendCommand _ = False

collectSends :: [ProcCommand] -> [(Int,[[Command]])]
collectSends = map toPair' 
             . groupWith fst 
             . map toPair 
             . groupWith destinationAndConnection 
             . filter (isSendCommand . snd . snd) 
             . attachProcDestination
  where
    destinationAndConnection (i,(_,Send cid _ _)) = (i,cid) 
    destinationAndConnection _ = error "Props.destinationAndConnection: unexpected value"

    toPair ls@((Just i,_):_) = (i,map (snd . snd) ls)
    toPair _ = error "Props.collectSends: unexpected value"

    toPair' ls@((i,_):_) = (i,map snd ls)
    toPair' _ = error "Props.collectSends toPair': unexpected value"

attachProcDestination :: [ProcCommand] -> [(Maybe Int,ProcCommand)]
attachProcDestination = go M.empty
  where go m (c@(_,ConnectTo _ pid cid _):cms) = (Nothing,c) : go (M.insert cid pid m) cms
        go m (c@(_,Send cid _ _):cms) = (M.lookup cid m,c) : go m cms
        go m (c:cms) = (Nothing,c) : go m cms
        go m [] = []

collectRecvs :: [[Response]] -> [[[Response]]]
collectRecvs = map (groupWith getConn) . map (filter isRecvResp)
  where
    isRecvResp (Recv _ _) = True
    isRecvResp _ = False
    getConn (Recv cid _) = cid
    getConn _ = error "Props.collectRecvs: unexpected value."


matchSendRecvs :: [(Int,[[Command]])] -> [[[Response]]] -> Bool
matchSendRecvs [] _ = True
matchSendRecvs ((i,ss):cms) rs = match' ss (rs!!i) && matchSendRecvs cms rs
  where match' ss' rs' = length ss' == length rs' && and (zipWith match'' ss' rs')
        match'' ss'' rs'' = length ss'' == length rs'' && and (zipWith matchCR ss'' rs'')
        matchCR (Send cid _ msg) (Recv cid' msg') = cid == cid' && msg == msg'
        matchCR _ _ = error "Props.matchSendRecvs: unexpected values"



singleTest :: IO ()
singleTest = runCommands test >>= print

writeCTest :: IO ()
writeCTest = writeFile "t.c"$ generateCTest test


test :: [ProcCommand]
test = [([0],Accept 5),([1],ConnectTo "" 0 5 Nothing),([1,0],WaitConnection 5),([1],Send 5 0 (Msg 0 185)),([0],WaitRecv 5 0),([1],Send 5 1 (Msg 1 969)),([1],WaitSendCompletion 5 0),([1],WaitSendCompletion 5 1),([1],Send 5 2 (Msg 2 495)),([1],WaitSendCompletion 5 2),([0],WaitRecv 5 1),([0],WaitRecv 5 2),([1],Disconnect 5)]

testR :: [[Response]]
testR = [[ReqAccepted 5,ConnectAccepted 5,Recv 5 (Msg 0 185),Recv 5 (Msg 2 495),Recv 5 (Msg 1 969)],[ConnectAccepted 5,SendCompletion 5 0 SUCCESS,SendCompletion 5 1 SUCCESS,SendCompletion 5 2 SUCCESS]]

