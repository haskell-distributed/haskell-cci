
import Control.Monad   ( when )
import System.Exit     ( exitFailure )

import TestGen         ( testProp, defaultTestConfig, runCommands, Command(..)
                       , ProcCommand, Response(..), TestConfig(..), generateCTest )



main :: IO ()
main = writeCTest -- singleTest -- props

props :: IO ()
props = do
    errs <- testProp (defaultTestConfig { nSends=20, nTries=400, nProcesses=3 })$ \cmds rs ->
                [ ( "sends equal completions"
                  , length (filter (isSendCommand . snd) cmds) == length (concatMap (filter isSendCompletion) rs) 
                  )
                ]
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
test = read "[([1],Accept 1),([0],ConnectTo \"\" 1 1 Nothing),([0,1],WaitConnection 1),([0],Disconnect 1),([1],Accept 0),([0],ConnectTo \"\" 1 0 Nothing),([0,1],WaitConnection 0),([0],Disconnect 0)]"

