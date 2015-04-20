module Math.RPS.RPS where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List
import Data.Csv
import Data.Maybe
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Sound.OSC.FD
import qualified Sound.Tidal.Stream as S (stream, name, params, Param(..) )
import qualified Sound.Tidal.Dirt as D
import Control.Concurrent

-- CSV helper for V3 conversion
instance (Show a) => ToRecord (V3 a) where
  toRecord (V3 x y z) = record $ map B.pack $ map (map replaceDot) $ map show [x, y, z]

replaceDot '.' = ','
replaceDot x = x


-- Dirt Bridge

dirtBridge port = do
  x <- udpServer "127.0.0.1" port
  c <- openUDP "127.0.0.1" 7771 -- dirt
  rotationM <- newMVar (0.0, 0, 0, 0)
  forkIO $ loop x c rotationM
  return $ (client, rotationClient)
    where loop x c rotationM = do m <- recvMessage x
                                  act m c rotationM
                                  loop x c rotationM
          act (Just (Message "/play" (sec:usec:cps:params))) c rotationM = do
                maybeRot <- tryReadMVar rotationM
                case maybeRot of
                  Just (rad, ix, iy, iz) -> do
                    let decodedParams = decodeDirtParams params
                        updatedParams = rotateDirtParamsBy (ix, iy, iz) rad decodedParams
                    sendOSC c $ Message "/play" $ [sec, usec, cps] ++ (map paramToDatum updatedParams)
                    return ()
                  Nothing -> return()
                return ()
          act (Just (Message "/rotate" (degrees:x:y:z:rest))) c rotationM = do
                let v = (fromJust $ d_get degrees) :: Int
                    x' = (fromJust $ d_get x) :: Int
                    y' = (fromJust $ d_get y) :: Int
                    z' = (fromJust $ d_get z) :: Int
                rad <- rotRadians v
                swapMVar rotationM (rad, x', y', z')
                return ()
          act m c rotationM = do
                putStrLn ("Unknown message: " ++ (show m))
                return ()
          rotRadians x = return $ ((fromIntegral x) / 360) * (pi*2)
          client = dirtBridgeStream D.dirt port
          rotationClient = openUDP "127.0.0.1" port

rotateParams s degrees paramIds = do
  let params = catMaybes $ map (\i -> elemIndex i dirtKeynames) paramIds
  
  sendOSC s $ Message "/rotate" ([int32 degrees] ++ (map int32 params))

dirtBridgeStream shape port = S.stream "127.0.0.1" port shape

dirtKeynames = map (S.name) (S.params D.dirt)
               

data RParam = RF { fvalue :: Float } | RI { ivalue :: Int } | RS { svalue :: String } | RDefault { dparam :: RParam }

getParam params x = params !! x

getParamsOrigin x y z = map (\index -> paramOrigin !! index) [x, y, z]


paramToDatum (RF x) = float x
paramToDatum (RI x) = int32 x
paramToDatum (RS x) = string x
paramToDatum (RDefault x) = paramToDatum x

getParamValue (RF x) = x
getParamValue (RI x) = fromIntegral $ x
getParamValue (RS x) = 0.0 -- ignore strings
getParamValue (RDefault x) = 0.0

setParamValue (RF x) v = RF v
setParamValue (RI x) v = RI (floor $ v)
setParamValue (RS x) v = RS x -- passthru value
setParamValue (RDefault (RF x)) v = RF x
setParamValue (RDefault (RI x)) v = RI x
setParamValue (RDefault (RS x)) v = RS x

rotateDirtParamsBy (ix, iy, iz) angle params = let params' = fromList params
                                                   [ox, oy, oz] = getParamsOrigin ix iy iz
                                                   [px, py, pz] = map (getParam params) [ix, iy, iz]
                                                   [x, y, z] = map getParamValue [px, py, pz]
                                                   (V3 x' y' z') = pMult (makeRotationMatrix angle ox oy oz) x y z
                                                   params'' = update ix (setParamValue px x') params'
                                                   params''' = update iy (setParamValue py y') params''
                                                   params'''' = update iz (setParamValue pz z') params'''
                                               in toList params''''    



decodeDirtParam (S.F _ (Just d)) x = let x' = ((fromJust $ d_get x) :: Float)
                                     in case (realToFrac d) == (realToFrac x') of
                                       True ->
                                         RDefault (RF x')
                                       False ->
                                         RF x'
decodeDirtParam (S.F _ Nothing) x = RF ((fromJust $ d_get x) :: Float)

decodeDirtParam (S.I _ (Just d)) x = let x' = ((fromJust $ d_get x) :: Int)
                                     in case d == x' of
                                       True ->
                                         RDefault (RI x')
                                       False ->
                                         RI x'
decodeDirtParam (S.I _ Nothing) x = RI ((fromJust $ d_get x) :: Int)



decodeDirtParam (S.S _ (Just d)) x = let x' = ((B.unpack $ fromJust $ d_get x) :: String)
                                     in case d == x' of
                                       True ->
                                         RDefault (RS x')
                                       False ->
                                         RS x'
decodeDirtParam (S.S _ Nothing) x = RS ((B.unpack $ fromJust $ d_get x) :: String)



decodeDirtParams params = zipWith (decodeDirtParam) (S.params D.dirt) params

-- FIXME: Improve origin values, think about scaling e.g. logarithmic for cutoff/hcutoff
paramOrigin = [
              0.0,
              0.0,
              0.5,
              0.5,
              1.0,
              0.5,
              0.0,
              0.0,
              0.5,
              0.5,
              0.0,
              0.5,
              0.0,
              1.0,
              0.0,
              0.5,
              0.5,
              0.5,
              0.0,
              0.0,
              0.5,
              0.5,
              0.5,
              0.5,
              0.0
              ]

-- core functions

makeRotationMatrix a x y z = fromQuaternion $ axisAngle (V3 x y z) a

pMult m x y z = m !* (V3 x y z)

-- recursive rotation
rotateUntil 0 matrixFunc x y z px py pz = []
rotateUntil 1 matrixFunc x y z px py pz= v' ++ [(pMult (matrixFunc x y z) px' py' pz')]
  where v' = rotateUntil 0 matrixFunc x y z px py pz
        px' = px
        py' = py
        pz' = pz



rotateUntil count matrixFunc x y z px py pz= v' ++ [(pMult (matrixFunc x y z) px' py' pz')]
  where v' = rotateUntil (count - 1) matrixFunc x y z px py pz
        latest = last v'
        (V3 px' py' pz') = latest


rotateTimes count (x, y, z) (px, py, pz) = rotateUntil count (makeRotationMatrix angle) x y z px py pz
  where angle = (pi*2) / count


-- utilities
        
v3ToTuple (V3 x y z) = (x, y, z)

writePoints fp count origin point = BS.writeFile fp $ encodeWith csvOptions $ rotateTimes count origin point
    where csvOptions  = defaultEncodeOptions {
                          encDelimiter = fromIntegral (ord '\t')
                        }

