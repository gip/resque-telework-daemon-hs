{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Redis as R
import Spawn
import HostInfo

import qualified Data.Aeson as A
import Data.List
import Data.Maybe
import Data.Either
import Data.Text as T
import Data.Time.Clock
import Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Typeable

import Control.Exception
import Control.Concurrent
import Control.Monad

import System.Posix.Process
import System.Exit
import System.Posix.IO
import System.IO
import System.Process
import System.Environment

daemon_version = "0.0.2-hs"

data Daemon = Daemon {
  redis :: R.Conn,
  host :: Text,
  version :: Text,
  delay :: Int,
  pid :: Text,
  ttl :: Integer,
  hostinfo :: Bool
}

data DaemonRT = DaemonRT {
  workers :: HM.HashMap Text A.Object,
  stop :: Bool
}

now = do
  t <- getCurrentTime
  return $ (pack . show) t

data Severity = Error | Info deriving (Show)

fromGenResp (R.V a) = a
fromAString (A.String s) = s
fromRight (Right a) = a
fromABool (A.Bool b) = b
--fromJust (Just a) = a

buildObject :: [(Text,Text)] -> HM.HashMap Text A.Value
buildObject = Data.List.foldr (\(k,v) a-> HM.insert k (A.String v) a) HM.empty



sendStatus d s t = do
  n <- now
  h <- return $ buildObject [("host", host d),("message", t),("date", n),("severity", pack $ show s)]
  putStrLn . T.unpack $ T.concat ["Telework: ", pack $ show s, ": ", t]
  R.pushStatus (redis d) (host d) h
  return ()
  where 
    add f v h = return $ HM.insert f (A.String v) h


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

checkProcess d drt = do
  workers' <- F.foldrM f HM.empty l
  return $ drt { workers = workers' }
  where
    l= HM.toList (workers drt)
    f (k,v) acc = do
      -- putStrLn $ show v
      st <- status $ read (unpack $ fromAString (v ! "pid"))
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
      (t, tt) <- do { t <- getCurrentTime; return (t, pack $ show t) }
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

startWorker d drt cmd = do
  re <- findRevision d r
  drt' <- case re of Just rev -> start rev
                     _ -> noRev r
  return drt'
  where
    r = cmd ! "revision"
    noRev r = do 
      sendStatus d Error (T.concat ["Unknown revision ", pack $ show r])
      return drt
    start rev = do
      pid <- spawn (Data.List.head exe) (Data.List.tail exe) (Just path) env fds
      sendStatus d Info (T.concat ["Starting worker ", id, " (PID ", pack $ show (fromRight pid), ")" ])
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
        unwrap h f = case h ! f of A.String s -> unpack s
                                   A.Number i -> show i
        unwrapD h f d = if (HM.member f h) then unwrap h f else d
        queue = unwrap cmd "queue"
        exe = Data.List.map unpack $ case cmd ! "exec" of A.String s -> splitOn " " s
        path = unwrap rev "revision_path"
        logp = pack $ unwrapD cmd "log_snapshot_period" "0"
        logl = pack $ unwrapD cmd "log_snapshot_lines" "0"
        logpath = unwrap rev "revision_log_path"
        logout = (logpath ++ "/telework_" ++ unpack id ++ "_stdout.log")
        logerr = (logpath ++ "/telework_" ++ unpack id ++ "_stderr.log")
        id= pack $ unwrap cmd "worker_id"
        env = [("QUEUE", queue),("BUNDLE_GEMFILE", path ++ "/Gemfile")]
        enva = A.Object (buildObject $ Data.List.map (\(a,b)->(pack a,pack b)) env)
        fds = [(stdInput, "/dev/null", ReadOnly),
               (stdOutput, logout, WriteOnly),
               (stdError, logerr, WriteOnly)]

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
  r <- case c of "start_worker" -> startWorker d drt cmd
                 "signal_worker" -> signalWorker d drt cmd
                 "stop_daemon" -> stopDaemon d drt
                 "kill_daemon" -> killDaemon d drt
                 _ -> do { sendStatus d Error (T.concat ["Unknown command ", fromAString c]); return drt }
  return r

loopBody :: Daemon -> DaemonRT -> IO DaemonRT
loopBody d drt = do
  iAmAlive d
  drt0 <- checkProcess d drt
  drt1 <- process drt0
  threadDelay (delay d)
  return drt1
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
    c <- return $ fromJust (R.decodeJ (R.textBs $ pack conf))
    r <- R.connect (fs "daemon_resque_prefix" c Nothing) (fs "daemon_telework_prefix" c Nothing)
                   (fsm "daemon_redis_host" c) (fim "daemon_redis_port" c)
    p <- getProcessID
    hi <- getHostInfo
    d <- return $ Daemon { pid= pack (show p), redis = r, host = h c, hostinfo = isJust hi,
                           version = daemon_version, delay = (dpi c)*1000*1000, ttl=10 }
    R.addHosts r (host d) 
    sendStatus d Info (T.concat ["Daemon (PID ", (pid d), ", version ", (version d),", native haskell) starting on host ", h c])
    mainLoop d (DaemonRT { workers = HM.empty, stop= False })
    sendStatus d Info (T.concat ["Daemon (PID ", (pid d), ", version ", (version d),", native haskell) exiting on host ", h c])  
    iAmDead d
    return ()
    where
      dpi c = case fim "daemon_pooling_interval" c of Just i -> i
                                                      Nothing -> 2 :: Int
      h c = pack $ fs "hostname" c Nothing
      fs k conf d = if HM.member k conf then case conf ! k of A.String s -> unpack s
                                        else fromJust d
      fsm k conf = if HM.member k conf then case conf ! k of A.String s -> Just $ unpack s
                                       else Nothing
      fim k conf = if HM.member k conf then case conf ! k of A.String i -> Just ((read $ unpack i) :: Int)
                                                             A.Number i -> Just (ceiling i)
                                       else Nothing
