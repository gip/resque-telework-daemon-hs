module Spawn where

import Data.Maybe
import System.Environment
import System.Directory
import System.Posix.Types
import System.Posix.IO
import System.Posix.Files
import System.Posix.Process
import System.Posix.Signals
import Control.Monad
import Control.Exception

-- Spawn a new task
spawn :: String                         -- Command name
      -> [String]                       -- Args
      -> Maybe FilePath                 -- New directory
      -> [(String, String)]             -- Additional environment vars
      -> [(Fd, String, OpenMode)]       -- Descriptors to redirect (likely to be stdin, out and err)
      -> IO (Either String Int)         -- A process ID if call is successful
spawn s args dir env std = do
  e <- getEnvironment  
  p <- forkProcess $ child s (env ++ e)
  return $ Right (fromIntegral p)
  where
    child :: String -> [(String, String)] -> IO ()
    child s e = do when (isJust dir) $ setCurrentDirectory $ fromJust dir
                   mapM_ (\(fd,n,m) -> do { closeFd fd; fd2 <- openFd n m (Just ownerModes) defaultFileFlags; dupTo fd fd2 }) std
                   createSession
                   executeFile s True args (Just e) -- This will never return!

data Status = ChildStatus (Maybe ProcessStatus)
            | NoChildStatus deriving Show

-- Check the status
status :: Int -> IO Status
status pid =
  catch 
    (liftM ChildStatus $ getProcessStatus False False (fromIntegral pid))
    (\e -> let _ = e :: IOException in return NoChildStatus)

-- Signal process
signal :: Int -> Int -> IO ()
signal sig pid = signalProcess (fromIntegral sig) (fromIntegral pid)

stringSignal :: String -> Int
stringSignal s = fromIntegral (tS s)
  where
   tS "KILL" = sigKILL
   tS "CONT" = sigCONT
   tS "QUIT" = sigQUIT
   tS "USR1" = sigUSR1
   tS "USR2" = sigUSR2





