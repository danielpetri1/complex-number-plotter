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

---- Complex numbers
i = 0.0 :+ 1.0
zero = 0.0 :+ 0.0
one = 1.0 :+ 0.0
two = 2.0 :+ 0.0

-- Complex functions
-- x^2
square z = cmul z z

-- Uses partial application to return a function g(x, c) = c + f(x)
cPlus c = cadd (c :+ 0.0)

-- x^2 + 25
squarePlus25 z = cadd (25 :+ 0) $ cmul z z

-- x = i
imagPlane _ = i

-- 2x**2 + 2x + 2
complexPoly z = cadd ((two `cmul` z) `cadd` two) (cmul two (square z))

format (a, b, c, d) = show a ++"\t" ++ show b ++"\t" ++ show c ++ "\t" ++ show d ++ "\n\n"

plot f ss a b = do
          let axis = [a, a+ss..b]
          let graph = [(x, y, magnitude $ f (x :+ y), phase $ f (x :+ y)) | x <- axis, y <- axis]

          writeFile "data.txt" $ foldr (++) "" $ map format graph

          gnuplot <- callCommand "gnuplot \"Surface-Magnitude.plt\""
          return gnuplot

{--
Changes the parameter c of a complex function f(x) + c in the interval [a, b] with a given
stepsize ss.

The method returns a list all functions where c set according to the interval above.
--}

functionList ss a b = [cPlus c | c <- [a, a+ss..b]]

-- TODO
-- delete folder content before starting
-- true main function, sanitize input

animate fx = animate' fx 0 where
  animate' [] n = putStrLn $ "Done, " ++ show n ++ " frames generated."
  animate' (f:fs) n = do
                    current <- plot (f . square) 0.125 (-10) (10)
                    return current

                    rename <- callCommand $ "ren \"pics\\output.png\" \"frame" ++ show n ++ ".png"
                    return rename

                    animate' fs (n+1)
