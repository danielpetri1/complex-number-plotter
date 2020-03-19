module ComplexPlotter where

import System.IO()
import Data.Complex
import Graphics.Gnuplot.Simple
import System.Random


{-
instance (Num a, Ord a, Show a) => Show (Complex a) where
    show (a :+ b)
      | a == 0 && b == 0 = "0"
      | b == 0 = show a
      | a == 0 = if b == 1 then "i" else if b == -1 then "-i" else show b ++ "i"
      | b > 0 = show a ++ " + " ++ show b ++ "i"
      | otherwise = show a ++ " - " ++ show (-1*b) ++ "i"
-}

-- Converts a number to a complex number
toComplex n = n :+ 0

-- Adds two complex numbers
cadd (a :+ b) (c :+ d) = (a + c) :+ (b + d)

-- Subtracts two complex numbers
csub (a :+ b) (c :+ d) = (a - c) :+ (b - d)

-- Multiplies two complex numbers
cmul (a :+ b) (c :+ d) = (a*c - b*d) :+ (a*d + b*c)

-- Calculates z^n (n >= 0)
cpow :: Num a => Complex a -> Integer -> Complex a
cpow z n = cpow' z n (1 :+ 0)
  where
    cpow' z n res
      | n <= 0 = res
      | otherwise = cpow' z (n-1) (cmul z res)

---- Complex numbers
i = 0.0 :+ 1.0
zero = 0.0 :+ 0.0
one = 1.0 :+ 0.0
two = 2.0 :+ 0.0

-- Complex functions
-- x^2
square z = cmul z z

-- x^2 + 25
squarePlus25 z = cadd (25 :+ 0) $ cmul z z

-- x = i
complexPoly _ = i

-- 2x**2 + 2x + 2
complexPoly2 z = cadd ((two `cmul` z) `cadd` two) (cmul two (square z))

-- Generate discrete data: returns (complex plane (z = a + bi), magnitude)
plot f z = (realPart z, imagPart z, magnitude $ f z, phase z)

-- Format expects a tuple of numbers (x, y, z) and formats it to gnuplot's style
format [] = ""
format ((x, y, z, p):xs) = show x ++ "\t" ++ show y ++ "\t" ++ show z ++ "\t" ++ show p ++ "\n\n" ++ format xs

interval a b ss
  | a > b = []
  | otherwise = a : interval (a+ss) b ss

-- Generates a grid-like pattern complex numbers in the range (a, b) with a given step size
grid ss a b = let zs = (interval a b ss) in grid' zs zs zs
  where
    grid' [] [] _ = []
    grid' (_:_) [] _ = []
    grid' [] (_:ys) zs = grid' zs ys zs
    grid' (x:xs) (y:ys) zs = (x :+ y) : grid' xs (y:ys) zs

output f = map (plot f)

main f = do
  putStr "Step size: "
  ss <- readLn :: IO Double
  putStr "a: "
  a <- readLn :: IO Double
  putStr "b: "
  b <- readLn :: IO Double
  writeFile ("data.txt") (format $ output f (grid ss a b))

-- Generates a random list of n complex numbers in the range (a, b)
randomComplex :: (Eq a, Num a, Random b) => a -> b -> b -> [Complex (IO b)]
randomComplex 0 _ _ = []
randomComplex n a b = (randomRIO (a, b) :+ randomRIO (a,b)) : randomComplex (n-1) a b

main' :: IO ()
main' = do
  plotFunc [] (linearScale 1000 (-10.0::Double,10.0)) (\x -> x^2)
  putStrLn "Press enter to exit."
  getLine
  return ()

a = let xs = [-2,-1.8..2::Double] in
      plotMesh3d [] [] (do x <- xs; return (do y <- xs; return (x,y,cos(x*x+y*y))))

-- Write generated data to file
mainRandom :: (Show c, RealFloat c) => (Complex Double -> Complex c) -> IO ()
mainRandom f = do
  putStr "Discrete data amount: "
  n <- readLn :: IO Integer
  putStr "a: "
  a <- readLn :: IO Double
  putStr "b: "
  b <- readLn :: IO Double
  write f n a b
    where
      write f n a b = apply f (randomComplex n a b) []
        where
          apply _ [] res = writeFile ("data.txt") (format res)
          apply f (x:xs) res =
            do
              real <- realPart x
              imag <- imagPart x
              apply f xs (plot f (real :+ imag) : res)
