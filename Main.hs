import Pages
import Pointset
import Prelude hiding ((<>))
import Data.Maybe (fromJust)
import Control.Monad

import Numeric.LinearAlgebra (fromLists, toLists, pinv, (<>))

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map  as Map

type VCoord = (Double, Double)
type Coord = (Double, Double)
type Mapo = (String, Char, Int)

training = concat [stations, museums, stadiums]
origin = (-23.55039, -46.63395) :: Coord

vCoordPage :: [[Text.Text]]
vCoordPage = [
  [Text.toUpper $ Text.strip $ Text.pack page | page <- row]
  | row <- pages]

pageVCoord :: Map.Map Text.Text (Int, Int)
pageVCoord =
  Map.fromList [
  (page, (i, j))
  | (i, row)  <- zip [0..] vCoordPage,
    (j, page) <- zip [0..] row
  ]

alphabet    = "ABCDEFHJLMNOPRSTUVXZ"
alphabetInv = Map.fromList $ zip alphabet [0..]

vcoordFromMapo :: Mapo -> VCoord
vcoordFromMapo (page, letter, number) = (y, x)
  where
    yx = fromJust $ flip Map.lookup pageVCoord $ Text.pack page
    y = (fromIntegral $ fst yx) +
        (fromIntegral number + 0.5) / 30
    x = (fromIntegral $ snd yx) +
        (0.5 + (fromJust $ Map.lookup letter alphabetInv)) /
        (fromIntegral $ length alphabet)

mapoFromVCoord :: VCoord -> Mapo
mapoFromVCoord (y, x) = (page, letter, number)
  where
    (i, j) = (floor y, floor x)
    (rest_i, rest_j) = (y - fromIntegral i, x - fromIntegral j)

    number = round $ rest_i * 30 + 0.5
    letter = alphabet !! (round $ rest_j * (fromIntegral $ length alphabet) - 0.5)
    page   = Text.unpack $ vCoordPage !! i !! j

coordToMeters :: Coord -> Coord -> (Double, Double)
coordToMeters origin dest = (dLat, dLng)
  where
    r      = 6371000            -- Earth radius
    latLen = 10001965.729       -- Length of a lat degree, in meters

    radians deg = deg/180 * pi

    meanDegLen latA latB =
      ((pi * r / 180) *
       ((sin $ radians latB) - (sin $ radians latA)) /
       (latB - latA))

    delta = (fst origin - fst dest, snd origin - snd dest)
    dLat = fst delta * latLen
    dLng = snd delta * meanDegLen (fst origin) (fst dest)

pRealDelta = [
  let
    (x, y) = coordToMeters origin $ snd p
  in [1.0 :: Double, x, y]
  | p <- training]
pVCoord = [
  let
    (x, y) = vcoordFromMapo $ fst p
  in [x, y]
  | p <- training]

r = fromLists pRealDelta
v = fromLists pVCoord
w = (pinv r) <> v -- Linear Regression

mapoFromCoord :: Coord -> Mapo
mapoFromCoord (y, x) =
  mapoFromVCoord (v !! 0, v !! 1)
  where
    (delta_a, delta_b) = coordToMeters origin (y, x)
    r = fromLists [[1, delta_a, delta_b]]
    v = toLists (r <> w) !! 0
  
coordFromStr :: String -> Coord
coordFromStr s = (t !! 0, t !! 1)
  where
    t = map (read . Text.unpack . Text.strip) $ Text.splitOn (Text.pack ",") $ Text.pack s :: [Double]

main :: IO ()
main = forever $ do
  inputStr <- getLine
  putStrLn $
    let
      (p, l, n) =  mapoFromCoord $ coordFromStr inputStr
    in p ++ " " ++ [l] ++ " " ++ show n
