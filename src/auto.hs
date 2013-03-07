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
import qualified Data.Vector as V


startAuto d drt cmd rev = do
  sendStatus d Error (T.concat ["Auto mode disabled on this daemon" ])
  return drt

--
--
startAuto' d drt cmd rev = do
  sendStatus d Error (T.concat ["Auto mode disabled on this daemon" ])
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
      (t, tt) <- do { t <- getCurrentTime; return (t, pack $ show t) }
      v' <- if ((diffUTCTime t last) - fromIntegral autop) >= 0 then do
      	putStrLn "."
      	--v' <- g (k,v)
        return $ HM.insert "last_action" (A.String tt) v -- Update it if
      else return v      
      return $ HM.insert k v' acc
      where
        last = read (cs $ fromAString (v ! "last_action"))
        autop = read (fromANumber (v ! "auto_delay")) :: Int
    g (k,v) = v

-- status
--
status d drt id =
  V.foldr f (0,0,0,0) worker_id
  where
  	worker_id = fromAArray ( ((autos drt) ! id) ! "worker_id" )
  	f ewid (run,void,unknown,count) =
  	  if HM.member wid (workers drt) then
  	    case ((workers drt) ! wid) ! "status" of A.String("RUN") -> (run+1,void,unknown,count+1)
  	                                             _ -> (run,void,unknown+1,count+1)
  	  else (run,void+1,unknown,count+1)
  	  where
  	  	wid = fromAString ewid



