{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Text as T
import Data.List as L
import Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.String.Conversions
import Data.Foldable as F

import qualified Redis as R
import qualified Data.Aeson as A

daemon_version = "0.0.3-hs"

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
  autos :: HM.HashMap Text A.Object,
  stop :: Bool
}

data Severity = Error | Info deriving (Show)


-- Fold on HashMap
-- Should be improved not to use a list even though laziness shouldn't make it too expensive to go through a list
hmFoldrM f hm = F.foldrM f HM.empty (HM.toList hm)

fromGenResp (R.V a) = a
fromAString (A.String s) = s
fromANumber (A.Number i) = show i
fromAArray (A.Array a) = a
fromRight (Right a) = a
fromABool (A.Bool b) = b

buildObject :: [(Text,Text)] -> HM.HashMap Text A.Value
buildObject = L.foldr (\(k,v) a-> HM.insert k (A.String v) a) HM.empty

now = do
  t <- getCurrentTime
  return $ (pack . show) t

sendStatus d s t = do
  n <- now
  h <- return $ buildObject [("host", host d),("message", t),("date", n),("severity", cs $ show s)]
  putStrLn . T.unpack $ T.concat ["Telework: ", cs $ show s, ": ", t]
  R.pushStatus (redis d) (host d) h
  return ()
  where 
    add f v h = return $ HM.insert f (A.String v) h