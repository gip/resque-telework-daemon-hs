{-# LANGUAGE OverloadedStrings #-}
module Worker where

import Common
import Spawn
import qualified Redis as R

import Data.Text as T
import Data.List as L
import Data.HashMap.Strict as HM
import Data.String.Conversions
import qualified Data.Aeson as A
import Data.Foldable as F
import Data.Time.Clock

import System.Posix.Process
import System.Exit
import System.Posix.IO
import System.IO
import System.Process



startWorker d drt cmd rev = do
  pid <- spawn (L.head exe) (L.tail exe) (Just path) env fds
  sendStatus d Info (T.concat ["Starting worker ", id, " (PID ", cs $ show (fromRight pid), ")" ])
  t <- now
  worker <- return $ buildObject [("pid", pack $ show (fromRight pid)),("status", "RUN"),
                                  ("log_snapshot_period", logp),("log_snapshot_lines", logl), 
                                  ("log_file_stdout", pack logout), ("log_file_stderr", pack logerr),
                                  ("log_snapshot_last", t)]
  worker <- return $ HM.insert "environment" enva worker
  R.addWorkers (redis d) (host d) id worker (ttl d)
  drt' <- return $ drt { workers = HM.insert id worker (workers drt) }
  return drt'
  where
    unwrap h f = case h ! f of A.String s -> cs s
                               A.Number i -> show i
    unwrapD h f d = if (HM.member f h) then unwrap h f else d
    queue = unwrap cmd "queue"
    exe = L.map unpack $ case cmd ! "exec" of A.String s -> splitOn " " s
    path = unwrap rev "revision_path"
    logp = pack $ unwrapD cmd "log_snapshot_period" "0"
    logl = pack $ unwrapD cmd "log_snapshot_lines" "0"
    logpath = unwrap rev "revision_log_path"
    logout = (logpath ++ "/telework_" ++ cs id ++ "_stdout.log")
    logerr = (logpath ++ "/telework_" ++ cs id ++ "_stderr.log")
    id= pack $ unwrap cmd "worker_id"
    env = [("QUEUE", queue),("BUNDLE_GEMFILE", path ++ "/Gemfile")]
    enva = A.Object (buildObject $ L.map (\(a,b)->(pack a,pack b)) env)
    fds = [(stdInput, "/dev/null", ReadOnly),
           (stdOutput, logout, WriteOnly),
           (stdError, logerr, WriteOnly)]


signalWorker d drt cmd = do
  sendStatus d Info (T.concat ["Signaling worker ", wid," with signal ", sig])
  signal (stringSignal $ unpack sig) (read $ unpack pid)
  return drt'
  where
    sig = case fromAString (cmd ! "action") of "PAUSE" -> "USR2"
                                               s -> s
    wid = fromAString(cmd ! "worker_id")
    worker = (workers drt) ! wid
    pid = fromAString (worker ! "pid")
    sta = case sig of "CONT" -> "RUN"
                      "USR2" -> "PAUSE"
                      s -> s
    drt' = drt { workers = HM.insert wid (HM.insert "status" (A.String sta) worker) (workers drt) }


checkWorker d drt = do
  workers' <- hmFoldrM f (workers drt)
  return $ drt { workers = workers' }
  where
    f (k,v) acc = do
      st <- status $ read (cs $ fromAString (v ! "pid"))
      case st of ChildStatus Nothing -> do -- Process running
                                           v' <- log k v
                                           R.addWorkers (redis d) (host d) k v' (ttl d)
                                           return $ HM.insert k v' acc
                 ChildStatus (Just _) -> do -- Process exited or terminated
                                            if sr then (sendStatus d Info (T.concat ["Worker ", k," has exited"])) 
                                                  else (sendStatus d Error (T.concat ["Worker ", k," has unexpectedly exited"]))
                                            R.remWorkers (redis d) (host d) k
                                            return acc
                                            where
                                              sr = case fromAString (v ! "status") of "KILL" -> True
                                                                                      "QUIT" -> True
                                                                                      _ -> False
                 NoChildStatus -> do -- It's not a child, not sure what happened
                                    (sendStatus d Error (T.concat ["Worker ", k," status unknown"])) 
                                    return acc
    log k v = do
      (t, tt) <- do { t <- getCurrentTime; return (t, cs $ show t) }
      v' <- if ((diffUTCTime t last) - fromIntegral logp) >= 0 then do
              lo <- tail logl (unpack $ fromAString (v ! "log_file_stdout"))
              le <- tail logl (unpack $ fromAString (v ! "log_file_stderr"))
              logs <- return $ buildObject [("date", tt),("log_stderr", pack le),("log_stdout", pack lo)]
              R.addLogs (redis d) (host d) k logs      
              return $ HM.insert "log_snapshot_last" (A.String tt) v
            else return v
      return v'
      where
        logp = read (unpack $ fromAString (v ! "log_snapshot_period")) :: Int
        logl = unpack $ fromAString (v ! "log_snapshot_lines")
        last = read (unpack $ fromAString (v ! "log_snapshot_last"))
    tail l fn = do
      (_, s , _) <- readProcessWithExitCode "tail" ["-n", l, fn] "" 
      return s

