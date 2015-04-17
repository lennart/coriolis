module Math.RPS.RPS where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Csv
import Linear.Matrix
import Linear.Quaternion
import Linear.V3


instance (Show a) => ToRecord (V3 a) where
  toRecord (V3 x y z) = record $ map B.pack $ map (map replaceDot) $ map show [x, y, z]

replaceDot '.' = ','
replaceDot x = x

makeRotationMatrix a x y z = fromQuaternion $ axisAngle (V3 x y z) a

pMult m x y z = m !* (V3 x y z)

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

          
v3ToTuple (V3 x y z) = (x, y, z)

writePoints fp count origin point = BS.writeFile fp $ encodeWith csvOptions $ rotateTimes count origin point
    where csvOptions  = defaultEncodeOptions {
                          encDelimiter = fromIntegral (ord '\t')
                        }

