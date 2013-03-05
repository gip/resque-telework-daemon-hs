{-# LANGUAGE OverloadedStrings #-}
module HostInfo where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Text.Regex
import Control.Exception
import Control.Monad

getHostInfo :: IO (Maybe [(T.Text,T.Text)])
getHostInfo = do
  load <- readMatch "/proc/loadavg" (mkRegex "([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)")
  mem <- readMatch "/proc/meminfo" (mkRegex "MemTotal:\\s*([0-9]+).+MemFree:\\s*([0-9]+)")
  return $ case (load,mem) of (Nothing, Nothing) -> Nothing
                              (Just a, Nothing) -> Just (zip a loadi)
                              (Nothing, Just b) -> Just (zip b memi)
                              (Just a, Just b) -> Just (zip a loadi ++ zip b memi)
  where
    loadi = ["load_avg_1min", "load_avg_5min", "load_avg_15min"]
    memi = ["mem_total", "mem_free"]

readMatch f re = catches res [Handler (\ (ex :: SomeException) -> return Nothing)]
  where
    res = do
      s <- readFile f
      return $ (liftM (map T.pack) (matchRegex re s))

