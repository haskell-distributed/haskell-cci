
import Control.Monad   ( when )
import System.Exit     ( exitFailure )

import TestGen         ( testProp, defaultTestConfig )

main :: IO ()
main = do
    errs <- testProp defaultTestConfig "example prop" (\_tr _rs -> True)
    mapM_ print errs
    when (not (null errs)) exitFailure

