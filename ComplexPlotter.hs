module ComplexPlotter where

import Data.Complex
import System.IO()
import System.Process

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

format (a, b, c, d) = show a ++"\t" ++ show b ++"\t" ++ show c ++ "\t" ++ show d ++ "\n\n"

plot f ss a b = do
          let axis = [a, a+ss..b]
          let graph = [(x, y, magnitude $ f (x :+ y), phase $ f (x :+ y)) | x <- axis, y <- axis]

          writeFile "data.txt" $ foldr (++) "" $ map format graph

          gnuplot <- callCommand "gnuplot \"Surface-Magnitude.plt\""
          return gnuplot
