{-# LANGUAGE OverloadedStrings #-}

module Redis where

import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad    
import Database.Redis


data Conn = Conn {
  connection :: Connection,
  prefix :: T.Text,
  teleprefix :: T.Text
}

textBs = TE.encodeUtf8
bsText = TE.decodeUtf8
toLazy = BL.pack . B.unpack
fromLazy = B.pack . BL.unpack
objectString :: A.Object -> String
objectString = T.unpack . bsText . fromLazy . A.encode

data Obj = HOSTS | TASKS | WORKERS | IDS | QUEUES | STATUS | LAST_SEEN
         | ALIASES | COMMENTS | LOGS | AUTOS | ALIVE | CMDS | REVISIONS
  deriving (Show)

connect pre tele host (port :: Maybe Integer) =
  do c <- Database.Redis.connect dci  
     return Conn { connection= c, prefix= T.pack pre, teleprefix= T.pack tele }
  where
    dci= let d0= case host of Just h -> defaultConnectInfo { connectHost= h }
                              Nothing -> defaultConnectInfo in 
         let d1= case port of Just p -> d0 { connectPort= PortNumber (fromIntegral p) }
                              Nothing -> d0 in
         d1
 
key:: Conn -> Maybe T.Text -> Obj -> T.Text
key conn Nothing QUEUES = T.concat [prefix conn, ":", "queues"]
key conn Nothing obj = T.concat [teleprefix conn, ":", T.toLower . T.pack $ show obj]
key conn (Just h) obj = T.concat [teleprefix conn, ":host:", h, ":", T.toLower . T.pack $ show obj]

data GenResp a = V a                                  -- Value
               | ED                                   -- Decode Error
               | E B.ByteString deriving (Show)       -- Redis Error

generic :: Conn
        -> Redis (Either Reply a)
        -> (a -> b)
        -> IO (GenResp b)
generic conn func post =
  runRedis (connection conn) $ do
    r <- unboxm func
    return r
  where
    unbox (Right a) = V $ post a
    unbox (Left (Error s)) = E s
    unboxm = liftM unbox

decodeJ :: B.ByteString -> Maybe A.Object
decodeJ = A.decode . toLazy



-- IDS
getIds :: Conn -> IO (GenResp Integer)
getIds conn   = generic conn (incr (textBs $ key conn Nothing IDS)) (\x -> x)

-- ALIVE
getAlive :: Conn -> T.Text -> IO (GenResp (Maybe A.Object))
getAlive conn h = generic conn (get (textBs $ key conn (Just h) ALIVE)) (join . liftM decodeJ)
addAlive :: Conn -> T.Text -> A.Object -> Integer -> IO (GenResp ())
addAlive conn h v ttl = generic conn (do { r <- set k vbs; _ <- expire k ttl; return r}) (\_ -> ())
  where k = (textBs $ key conn (Just h) ALIVE)
        vbs = (fromLazy . A.encode) v
remAlive conn h = generic conn (del [(textBs $ key conn (Just h) ALIVE)]) (\_ -> ())

-- HOSTS
getHosts conn = generic conn (smembers (textBs $ key conn Nothing HOSTS)) (map bsText)
addHosts conn k = generic conn (sadd (textBs $ key conn Nothing HOSTS) [textBs k]) (\x -> x)
remHosts conn k = generic conn (srem (textBs $ key conn Nothing HOSTS) [textBs k]) (\x -> x)

-- Generic function to access hashes
--get_ :: Obj -> Conn -> T.Text -> IO (GenResp [(T.Text, Maybe A.Object)])
get_ obj conn h = generic conn (hgetall (textBs $ key conn (Just h) obj)) (map (\(a,b) -> (bsText a, decodeJ b)))
--get_by_id :: Obj -> Conn -> T.Text -> T.Text -> IO (GenResp (Maybe A.Object))
get_by_id obj conn h id= generic conn (hget (textBs $ key conn (Just h) obj) (textBs id)) (join . liftM decodeJ)
set_ :: Obj -> Conn -> T.Text -> T.Text -> A.Object -> IO (GenResp Bool)
set_ obj conn h id v = generic conn (hset k (textBs id) vbs) (\x -> x)
  where k = (textBs $ key conn (Just h) obj)
        vbs= (fromLazy . A.encode) v

-- WORKERS
getWorkers = get_ WORKERS
getWorkersById = get_by_id WORKERS
remWorkers conn h id = generic conn (hdel (textBs $ key conn (Just h) WORKERS) [(textBs id)]) (\x -> x) 
addWorkers conn h id v ttl = generic conn (do { r <- hset k (textBs id) vbs; expire k ttl }) (\x -> x)
  where k = (textBs $ key conn (Just h) WORKERS)
        vbs= (fromLazy . A.encode) v

-- LOGS
addLogs = set_ LOGS

-- TASKS
getTasks = get_ TASKS
getTasksById = get_by_id TASKS 

-- AUTOS
getAutos = get_ AUTOS
getAutosById = get_by_id AUTOS 

-- QUEUES
getQueues conn = generic conn (smembers (textBs $ key conn Nothing QUEUES)) (map bsText)

-- ALIASES
getAliases conn k = generic conn (hget (textBs $ key conn Nothing ALIASES) (textBs k)) (liftM bsText) 

-- CMDS
popCmds conn h = generic conn (rpop (textBs $ key conn (Just h) CMDS)) (join . liftM decodeJ)

-- LAST_SEEN
setLastSeen conn h v = generic conn (set (textBs $ key conn (Just h) LAST_SEEN) (textBs v)) (\_ -> ())

-- STATUS
pushStatus :: Conn -> T.Text -> A.Object -> IO (GenResp Integer)
pushStatus conn h v = generic conn (do { r <- lpush k vbs; _ <- ltrim k 0 100; return r}) (\x -> x)
  where k = (textBs $ key conn Nothing STATUS)
        vbs = [(fromLazy . A.encode) v]


-- REVISIONS
getRevisions :: Conn -> T.Text -> Integer -> IO (GenResp [Maybe A.Object])
getRevisions conn h max = generic conn (lrange (textBs $ key conn (Just h) REVISIONS) 0 max) (map decodeJ)

