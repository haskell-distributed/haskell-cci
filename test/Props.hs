
import Control.Monad   ( when, forM_ )
import System.Exit     ( exitFailure )

import TestGen         ( testProp, defaultTestConfig, runCommands, Command(..), Msg(..)
                       , ProcCommand, Response(..), TestConfig(..), generateCTest )



main :: IO ()
main = props -- writeCTest -- singleTest -- props

props :: IO ()
props = do
    errs <- testProp (defaultTestConfig { nSends=3
                                        , nTries=200
                                        , nProcesses=2
                                        , nMinMsgLen=200
                                        , nMaxMsgLen=300
                                        })$ \cmds rs ->
                [ ( "sends equal completions"
                  , length (filter (isSendCommand . snd) cmds) == length (concatMap (filter isSendCompletion) rs) 
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
    isSendCommand (Send _ _ _) = True
    isSendCommand _ = False
    isSendCompletion (SendCompletion _ _ _) = True
    isSendCompletion _ = False


singleTest :: IO ()
singleTest = runCommands test >>= print

writeCTest :: IO ()
writeCTest = writeFile "t.c"$ generateCTest test

test :: [ProcCommand]
test = [([0],Accept 0),([1],ConnectTo "" 0 0 Nothing),([1,0],WaitConnection 0),([1],Send 0 1 (Msg 1 250)),([1],WaitSendCompletion 0 1),([0],WaitRecv 0 1),([1],Disconnect 0)]

-- test1 = [([2],Accept 2),([1],ConnectTo "" 2 2 Nothing),([1,2],WaitConnection 2),([1],Send 2 20 (Msg 20 996)),([1],WaitSendCompletion 2 20),([2],WaitRecv 2 20),([1],Send 2 19 (Msg 19 690)),([1],WaitSendCompletion 2 19),([2],WaitRecv 2 19),([1],Send 2 18 (Msg 18 784)),([1],WaitSendCompletion 2 18),([1],Send 2 17 (Msg 17 949)),([2],WaitRecv 2 18),([2],WaitRecv 2 17),([1],Send 2 16 (Msg 16 942)),([1],WaitSendCompletion 2 17),([1],WaitSendCompletion 2 16),([1],Send 2 15 (Msg 15 552)),([1],Send 2 14 (Msg 14 990)),([1],WaitSendCompletion 2 15),([1],WaitSendCompletion 2 14),([1],Send 2 13 (Msg 13 634)),([1],WaitSendCompletion 2 13),([2],WaitRecv 2 16),([2],WaitRecv 2 15),([2],WaitRecv 2 14),([2],WaitRecv 2 13),([1],Send 2 12 (Msg 12 702)),([1],Send 2 11 (Msg 11 1019)),([1],WaitSendCompletion 2 12),([1],WaitSendCompletion 2 11),([2],WaitRecv 2 12),([2],WaitRecv 2 11),([1],Send 2 10 (Msg 10 706)),([1],WaitSendCompletion 2 10),([1],Send 2 9 (Msg 9 911)),([1],WaitSendCompletion 2 9),([2],WaitRecv 2 10),([2],WaitRecv 2 9),([1],Send 2 8 (Msg 8 960)),([2],WaitRecv 2 8),([1],Send 2 7 (Msg 7 952)),([1],WaitSendCompletion 2 8),([1],WaitSendCompletion 2 7),([1],Send 2 6 (Msg 6 760)),([2],WaitRecv 2 7),([2],WaitRecv 2 6),([1],Send 2 5 (Msg 5 744)),([1],WaitSendCompletion 2 6),([1],WaitSendCompletion 2 5),([1],Send 2 4 (Msg 4 935)),([1],WaitSendCompletion 2 4),([1],Send 2 3 (Msg 3 831)),([1],WaitSendCompletion 2 3),([2],WaitRecv 2 5),([2],WaitRecv 2 4),([2],WaitRecv 2 3),([1],Send 2 2 (Msg 2 956)),([1],WaitSendCompletion 2 2),([1],Send 2 1 (Msg 1 557)),([1],WaitSendCompletion 2 1),([2],WaitRecv 2 2),([2],WaitRecv 2 1),([1],Disconnect 2)]


