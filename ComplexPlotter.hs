module ComplexPlotter where

import Data.Complex
import Data.Maybe()
import System.IO()
import Graphics.Gnuplot.Simple
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

main f = do
          putStr "Step size: "
          ss <- readLn :: IO Double
          putStr "a: "
          a <- readLn :: IO Double
          putStr "b: "
          b <- readLn :: IO Double
          -- Discrete set of points spanned by (a, b)
          let axis = [a,a+ss..b]
          let phases = map phase $ map f [(x :+ y) | x <- axis, y <- axis] -- color

          writeFile "data.txt" (foldr (++) "" $ map (++"\n\n") $ map show $ phases)

          plotMesh3d [Grid (Just [""]),
                      XLabel "Real(x)",
                      ZLabel "|f(x)|",
                      YLabel "Imag(x)"] []
                      (do x <- axis;
                          return $ do
                                  y <- axis
                                  return $ (x, y, magnitude $ f (x :+ y)))
