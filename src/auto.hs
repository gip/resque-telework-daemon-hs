{-# LANGUAGE OverloadedStrings #-}
module Auto where

import Common
import qualified Redis as R

import Data.Text as T
import Data.List as L
import Data.HashMap.Strict as HM
import Data.String.Conversions
import qualified Data.Aeson as A
import Data.Foldable as F
import Data.Time.Clock

--
--
startAuto d drt cmd rev = do
  if HM.member id (autos drt) 
  	then do sendStatus d Error (T.concat ["Task ", id, "is already in auto mode"])
  	        return drt
    else do t <- now
    	    au' <- return $ HM.insert "last_action" (A.String t) au
    	    R.addAutos (redis d) (host d) id au' (ttl d)
    	    sendStatus d Info (T.concat ["Task ", id, " is now in auto mode"])
    	    return drt { autos = HM.insert id au' (autos drt) }
  where
  	id = fromAString (cmd ! "task_id")
  	au = HM.insert "rev_info" (A.Object rev) cmd

--
--
stopAuto d drt cmd = do
  sendStatus d Error (T.concat ["Auto mode disabled on this daemon" ])
  return drt

--
--
checkAuto d drt = do
  autos' <- hmFoldrM f (autos drt)
  return $ drt { autos = autos' }
  where
    f (k,v) acc = do
      R.addAutos (redis d) (host d) k v (ttl d)
      return $ HM.insert k v acc


