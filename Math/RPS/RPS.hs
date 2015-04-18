module Math.RPS.RPS where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Csv
import Data.Maybe

import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Sound.OSC.FD
import qualified Sound.Tidal.Stream as S (stream, name, params)
import qualified Sound.Tidal.Dirt as D
import Control.Concurrent

-- CSV helper for V3 conversion
instance (Show a) => ToRecord (V3 a) where
  toRecord (V3 x y z) = record $ map B.pack $ map (map replaceDot) $ map show [x, y, z]

replaceDot '.' = ','
replaceDot x = x


-- Dirt Bridge
paramOrigin = [
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

dirtBridge port = do
  x <- udpServer "127.0.0.1" port
  rotationM <- newMVar (0.0, 0, 0, 0)
  forkIO $ loop x rotationM
  return $ (client, rotationClient)
    where loop x rotationM = do m <- recvMessage x
                                act m rotationM
                                loop x rotationM
          act (Just (Message "/play" (sec:usec:cps:sound:params))) rotationM = do
                putStrLn ("Recv Play call with params: " ++ (show params))
                maybeRot <- tryReadMVar rotationM
                case maybeRot of
                  Just (rad, px, py, pz) -> do
                    let (ox, oy, oz) = (realToFrac $ paramOrigin !! px, realToFrac $ paramOrigin !! py, realToFrac $ paramOrigin !! pz)
                        (vx, vy, vz) = (params !! px, params !! py, params !! pz)
                        vx' = fromIntegral $ ((fromJust $ d_get vx) :: Int)
                        vy' = fromIntegral $ ((fromJust $ d_get vy) :: Int)
                        vz' = fromIntegral $ ((fromJust $ d_get vz) :: Int)
                        a = 0.5
                        b = 0.25
                        (V3 vx'' vy'' vz'') = pMult (makeRotationMatrix pi a a a) b b b
  --                  putStrLn ("Rotating: " ++ (show px) ++ (show py) ++ (show pz) ++ " by: " ++ (show rad) ++ "rad")
    --                putStrLn ("Rotated Point: " ++ (show vx'') ++ (show vy'') ++ (show vz''))
                    return ()
                
                    
                  Nothing -> return()
                return ()
          act (Just (Message "/rotate" (degrees:x:y:z:rest))) rotationM = do
                putStrLn ("Recv Rotate call with degrees: " ++ (show degrees) ++ " and paramIds: " ++ (show x) ++ (show y) ++ (show z))
                let v = (fromJust $ d_get degrees) :: Int
                    x' = (fromJust $ d_get x) :: Int
                    y' = (fromJust $ d_get y) :: Int
                    z' = (fromJust $ d_get z) :: Int
                rad <- rotRadians v
                swapMVar rotationM (rad, x', y', z')
                return ()
          rotRadians x = return $ ((fromIntegral x) / 360) * (pi*2)
          client = dirtBridgeStream D.dirt port
          rotationClient = openUDP "127.0.0.1" port

rotateParams s degrees paramIds = do
  sendOSC s $ Message "/rotate" ([int32 degrees] ++ (map int32 paramIds))

dirtBridgeStream shape port = S.stream "127.0.0.1" port shape




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

