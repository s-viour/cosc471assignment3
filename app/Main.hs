module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map

import Lib ( newtonInterpolant )

xs :: [Double]
xs = [6, 6.50000000000000, 7, 7.50000000000000, 8, 9, 10]

ys :: [Double]
ys = [1.43332941456034, 1.52577121960346, 1.63231621995538, 1.75505465696030, 1.89648087930495, 2.24790798667647, 2.71828182845905]

interpolationValues :: Map Double Double
interpolationValues = Map.fromList (zip xs ys)

f :: Double -> Double
f x = case Map.lookup x interpolationValues of
  Just v -> v
  Nothing -> error "invalid function value"

nInterp :: Double -> Double
nInterp = newtonInterpolant f xs

main :: IO ()
main = mapM_ (\x -> putStrLn $ (show x) ++ "," ++ (show $ nInterp x)) [0,0.5..12]
