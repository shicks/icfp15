module Debug (trace, setVerbose) where

import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )
import qualified Debug.Trace as D

verboseRef :: IORef Bool
verboseRef = unsafePerformIO $ newIORef False

isVerbose :: Bool
isVerbose = unsafePerformIO $ readIORef verboseRef

trace :: String -> a -> a
trace msg ret = if isVerbose then D.trace msg ret else ret

setVerbose :: Bool -> IO ()
setVerbose verbose = writeIORef verboseRef verbose
