--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--
--
-- This file implements an executable which runs automatically 
-- generated tests for the CCI haskell bindings.
--
import Control.Monad   ( when )
import qualified Data.Map as M( empty, insert, lookup )
import GHC.Exts        ( groupWith )
import System.Exit     ( exitFailure )

import TestGen         ( testProp, defaultTestConfig, runCommands, Command(..), Msg(..)
                       , ProcCommand, Response(..), TestConfig(..), generateCTest )
import Network.CCI    ( Status(..) )
import System.Console.CmdArgs

main :: IO ()
main =  writeFile "rmatest2.c" (generateCTest test2) -- singleTest -- props

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
                              , withValgrind = boolopt "Run tests with valgrind" (withValgrind defaultTestConfig) &= name "with-valgrind" &= explicit
                              , testRMA = boolopt "Test rma operations" (testRMA defaultTestConfig) &= name "test-rma" &= explicit
                              }
                                &= summary "CCI test generator v0.0.1, (C) Parallel Scientific 2012"
                                &= details ["See test/README for details."]
                                &= program "test_cci"
                  )
    errs <- testProp tc printErrorAndGenerateTest$ \cmds rs ->
                [ ( "sends equal completions"
                  , testRMA tc || length (filter (isSendCommand . snd) cmds) == length (concatMap (filter isSendCompletion) rs) 
                  )
                , ( "messages arrive sorted"
                  , testRMA tc || matchSendRecvs (collectSends cmds) (collectRecvs rs)
                  )
                ]
    when (not (null errs))$ exitFailure
  where
    isSendCompletion (SendCompletion _ mid _) = mid>0
    isSendCompletion _ = False
    intopt :: (Default a,Data a,Show a) => String -> a -> a
    intopt s i = i &= help (s++" [default: "++show i++"]") &= opt i
    boolopt :: (Default a,Data a,Show a) => String -> a -> a
    boolopt s i = i &= help (s++" [default: "++show i++"]")

    printErrorAndGenerateTest e@(s,_,_) i = do
      print e
      let cfilename :: Int -> String
          cfilename i = "t"++show i++".c"
      writeFile (cfilename i)$ generateCTest s
      putStrLn$ "A test program has been written to " ++ show (cfilename i)


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
        go _ [] = []

collectRecvs :: [[Response]] -> [[[Response]]]
collectRecvs = map (groupWith getConn) . map (filter isRecvResp)
  where
    isRecvResp (Recv _ (Msg _ _)) = True
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
singleTest = runCommands test2 >>= print


test :: [ProcCommand]
test = [([0],Accept 5),([1],ConnectTo "" 0 5 Nothing),([1,0],WaitConnection 5),([1],Send 5 1 (Msg 1 185)),([0],WaitRecv 5 1),([1],Send 5 2 (Msg 2 969)),([1],WaitSendCompletion 5 1),([1],WaitSendCompletion 5 2),([1],Send 5 3 (Msg 3 495)),([1],WaitSendCompletion 5 3),([0],WaitRecv 5 2),([0],WaitRecv 5 3),([1],Disconnect 5)]

testR :: [[Response]]
testR = [[ReqAccepted 5,ConnectAccepted 5,Recv 5 (Msg 0 185),Recv 5 (Msg 2 495),Recv 5 (Msg 1 969)],[ConnectAccepted 5,SendCompletion 5 0 SUCCESS,SendCompletion 5 1 SUCCESS,SendCompletion 5 2 SUCCESS]]

test2 :: [ProcCommand]
test2 = [([0],Accept 1)
  ,([1],ConnectTo "" 0 1 Nothing)
  ,([1,0],WaitConnection 1)
  ,([0],RMAReuseRMAHandle 1)
  ,([1],RMAHandleExchange 1)
  ,([0],RMAHandleExchange 1)
  ,([1,0],RMAWaitExchange 1)
  ,([0],RMAPrepareRead 1 5)
  ,([1],RMARead 1 5)
  ,([1,0],RMAWaitRead 1 5)
--  ,([0],RMAPrepareRead 1 6)
--  ,([1],RMARead 1 6)
--  ,([1,0],RMAWaitRead 1 6)
--  ,([1],RMAWrite 1 7)
--  ,([1,0],RMAWaitWrite 1 7)
--  ,([1],RMAWrite 1 8)
--  ,([1,0],RMAWaitWrite 1 8)
  ,([1],RMAFreeHandles 1)
  ,([0],RMAFreeHandles 1)
  ,([1],Disconnect 1)
  ]

{-
test2 = [([0],Accept 1)
  ,([1],ConnectTo "" 0 1 Nothing)
  ,([1,0],WaitConnection 1)

  ,([1],RMAReuseRMAHandle 1)
  ,([1],RMAHandleExchange 1)
  ,([0],RMAHandleExchange 1)
  ,([1,0],RMAWaitExchange 1)
  ,([0],RMAPrepareRead 1 2)
  ,([1],RMARead 1 2)
  ,([1,0],RMAWaitRead 1 2)

  ,([1],RMAFreeHandles 1),([0],RMAFreeHandles 1),([1],Disconnect 1)]


 -}

test3 :: [ProcCommand]
test3 =
 [([1],Accept 0),([0],ConnectTo "" 1 0 Nothing),([0],Accept 1),([1],ConnectTo "" 0 1 Nothing),([1,0],WaitConnection 1),([1],RMAReuseRMAHandle 1),([0],RMAReuseRMAHandle 1),([1],RMAHandleExchange 1),([0,1],WaitConnection 0),([1],RMAReuseRMAHandle 0),([0],RMAHandleExchange 1),([1,0],RMAWaitExchange 1),([0],RMAHandleExchange 0),([1],Send 1 1 (Msg 1 16)),([1],RMAWrite 1 5),([1],Send 1 2 (Msg 2 16)),([1],RMAHandleExchange 0),([1,0],RMAWaitWrite 1 5),([1],RMAWrite 1 6),([0],Send 0 1 (Msg 1 16)),([0,1],RMAWaitExchange 0),([1],WaitSendCompletion 1 1),([1,0],RMAWaitWrite 1 6),([0],RMAPrepareRead 1 7),([0],WaitSendCompletion 0 1),([1],RMARead 1 7),([1,0],RMAWaitRead 1 7),([1],RMAWrite 1 8),([1],WaitRecv 0 1),([1],RMAPrepareRead 0 5),([0],RMARead 0 5),([0,1],RMAWaitRead 0 5),([1],WaitSendCompletion 1 2),([0],RMAWrite 0 6),([0],Send 0 2 (Msg 2 16)),([0],WaitSendCompletion 0 2),([1,0],RMAWaitWrite 1 8),([0],WaitRecv 1 1),([0],WaitRecv 1 2),([1],RMAFreeHandles 1),([1],Send 1 3 (Msg 3 16)),([0,1],RMAWaitWrite 0 6),([0],RMAFreeHandles 1),([0],RMAWrite 0 7),([0],Send 0 3 (Msg 3 16)),([0,1],RMAWaitWrite 0 7),([1],RMAPrepareRead 0 8),([0],RMARead 0 8),([0,1],RMAWaitRead 0 8),([0],RMAFreeHandles 0),([0],WaitRecv 1 3),([1],Send 1 4 (Msg 4 16)),([0],Send 0 4 (Msg 4 16)),([1],RMAFreeHandles 0),([1],WaitSendCompletion 1 3),([1],WaitSendCompletion 1 4),([0],WaitSendCompletion 0 3),([0],WaitSendCompletion 0 4),([1],WaitRecv 0 2),([0],WaitRecv 1 4),([1],WaitRecv 0 3),([1],WaitRecv 0 4),([1],Disconnect 1),([0],Disconnect 0)] 


