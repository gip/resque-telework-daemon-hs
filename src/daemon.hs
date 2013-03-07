{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Redis as R
import Worker
import Auto
import Spawn
import HostInfo
import Common

import qualified Data.Aeson as A
import Data.List
import Data.Maybe
import Data.Either
import Data.Text as T
import Data.Time.Clock
import Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Typeable
import Data.String.Conversions

import Control.Exception
import Control.Concurrent
import Control.Monad

import System.Posix.Process
import System.Exit
import System.Posix.IO
import System.IO
import System.Process
import System.Environment



iAmAlive :: Daemon -> IO ()
iAmAlive d = do
  t <- now
  h <- return [("date", t),("version", version d)]
  h' <- if hostinfo d then do hi <- getHostInfo
                              return $ case hi of Just i -> (i ++ h)
                                                  Nothing -> h
                      else return h
  R.addAlive (redis d) (host d) (buildObject h') 10
  R.setLastSeen (redis d) (host d) t
  return ()

iAmDead :: Daemon -> IO ()
iAmDead d = do
  t <- now
  R.setLastSeen (redis d) (host d) t
  R.remAlive (redis d) (host d)
  return ()

findRevision :: Daemon -> A.Value -> IO (Maybe A.Object)
findRevision d rev = do
  revs <- R.getRevisions (redis d) (host d) 100
  return (join $ Data.List.find f (fromGenResp revs))
  where
    f (Just obj) = rev==(obj ! "revision")
    f _ = False


stopDaemon d drt = do
  drt' <- if (HM.size $ workers drt) > 0 
          then do sendStatus d Error (T.concat ["Can't stop daemon (PID ", (pid d), ") on host ", host d, 
                                                " as worker(s) are still running"])
                  return drt
          else do sendStatus d Info (T.concat ["Stopping daemon (PID ", (pid d), ") on host ", host d])
                  return $ drt { stop= True }
  return drt'

killDaemon d drt = do
  sendStatus d Info (T.concat ["Hard stopping daemon (PID ", (pid d), ") on host ", host d, " by calling exit()"])
  iAmDead d
  exitSuccess
  return drt

-- Execute a command
doCmd :: Daemon -> DaemonRT -> A.Object -> IO DaemonRT
doCmd d drt cmd = do
  c <- return $ cmd ! "command"
  r <- case c of "start_worker" -> do re <- findRevision d (cmd ! "revision")
                                      case re of Just rev -> startWorker d drt cmd rev
                                                 Nothing -> return drt
                 "signal_worker" -> signalWorker d drt cmd
                 "stop_daemon" -> stopDaemon d drt
                 "kill_daemon" -> killDaemon d drt
                 "start_auto" -> do re <- findRevision d (cmd ! "revision")
                                    case re of Just rev -> startAuto d drt cmd rev
                                               Nothing -> return drt
                 "stop_auto" -> stopAuto d drt cmd
                 _ -> do { sendStatus d Error (T.concat ["Unknown command ", fromAString c]); return drt }
  return r

loopBody :: Daemon -> DaemonRT -> IO DaemonRT
loopBody d drt = do
  iAmAlive d
  drt0 <- checkWorker d drt
  drt1 <- process drt0
  drt2 <- checkAuto d drt1
  threadDelay (delay d)
  return drt2
  where
    process drt = do
      cmd <- R.popCmds (redis d) (host d)
      case cmd of R.V (Just a) -> do { drt' <- doCmd d drt a; process drt' }
                  _ -> return drt


mainLoop d drt = catches (loop drt) [
  Handler (\ (ex :: ExitCode) -> return ()),
  Handler (\ (ex :: SomeException) -> 
    do sendStatus d Error (T.concat ["Exception thrown in daemon on host ", host d])
       sendStatus d Error (T.concat ["Please submit a bit report for exception ", pack $ show ex]) ) 
  ]
  where loop drt = do drt' <- loopBody d drt
                      if (stop drt') then return () else loop drt'

main = do
  args <- getArgs
  if Data.List.length args /= 1 then do 
    putStrLn "Telework: Usage: telework-daemon <config file>"
    return ()
  else do
    conf <- readFile $ Data.List.head args
    c <- return $ fromJust (R.decodeJ (cs conf))
    r <- R.connect (fs "daemon_resque_prefix" c Nothing) (fs "daemon_telework_prefix" c Nothing)
                   (fsm "daemon_redis_host" c) (fim "daemon_redis_port" c)
    p <- getProcessID
    hi <- getHostInfo
    d <- return $ Daemon { pid= pack (show p), redis = r, host = h c, hostinfo = isJust hi,
                           version = cs daemon_version, delay = (dpi c)*1000*1000, ttl=10 }
    R.addHosts r (host d) 
    sendStatus d Info (T.concat ["Daemon (PID ", (pid d), ", version ", (version d),", native haskell) starting on host ", h c])
    mainLoop d (DaemonRT { workers = HM.empty, autos= HM.empty, stop= False })
    sendStatus d Info (T.concat ["Daemon (PID ", (pid d), ", version ", (version d),", native haskell) exiting on host ", h c])  
    iAmDead d
    return ()
    where
      dpi c = case fim "daemon_pooling_interval" c of Just i -> i
                                                      Nothing -> 2 :: Int
      h c = pack $ fs "hostname" c Nothing
      fs k conf d = if HM.member k conf then case conf ! k of A.String s -> cs s
                                        else fromJust d
      fsm k conf = if HM.member k conf then case conf ! k of A.String s -> Just $ unpack s
                                       else Nothing
      fim k conf = if HM.member k conf then case conf ! k of A.String i -> Just ((read $ unpack i) :: Int)
                                                             A.Number i -> Just (ceiling i)
                                       else Nothing
