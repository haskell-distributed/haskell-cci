
import Control.Monad   ( when )
import System.Exit     ( exitFailure )

import TestGen         ( testProp, defaultTestConfig, runCommands, Command(..)
                       , ProcCommand, Response(..), TestConfig(..), generateCTest )



main :: IO ()
main = props

props :: IO ()
props = do
    errs <- testProp (defaultTestConfig { nSends=2, nTries=1000 })  "sends equal completions"$
                \cmds rs -> length (filter (isSendCommand . snd) cmds) == length (concatMap (filter isSendCompletion) rs) 
    mapM_ print errs
    when (not (null errs)) exitFailure
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
-- test = read "[([0],Accept 0),([1],Accept 1),([1],ConnectTo \"\" 0 0 Nothing),([0],ConnectTo \"\" 1 1 Nothing),([1,0],WaitConnection 0),([0,1],WaitConnection 1),([0],Disconnect 1),([1],Disconnect 0)]"

test = read "[([1],Accept 1),([0],ConnectTo \"\" 1 1 Nothing),([0],Accept 0),([1],ConnectTo \"\" 0 0 Nothing),([1,0],WaitConnection 0),([0,1],WaitConnection 1),([0],Send 1 2 \"2\"),([0],WaitSendCompletion 1 2),([0],Send 1 1 \"1\"),([0],WaitSendCompletion 1 1),([1],Send 0 2 \"2\"),([1],Send 0 1 \"1\"),([1],WaitRecv 1 2),([1],WaitRecv 1 1),([1],WaitSendCompletion 0 2),([1],WaitSendCompletion 0 1),([0],WaitRecv 0 2),([0],Disconnect 1),([0],WaitRecv 0 1),([1],Disconnect 0)]"
