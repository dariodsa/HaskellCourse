-- =============================================================================== --
{- |
  Welcome to your fifth Haskell training. Get ready to rumble.
  Where will you let recursion lead you?

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-05`, create a Merge Request,
  and assign it to your TA.

  Keep in mind that **all exposed functions must have type signatures** (helper functions that are defined in local definitions don't)!

  As always, ask your TA if you need any help.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.List
import Data.Char
--

{- * 5.1 Recursive functions with accumulators -}

{-
 - For the following functions, we'd like you to to use the `seq` function to
 - reduce the accumulators to WHNF where needed to avoid building big thunks.
 -}

-- ** TE 5.1.1
--
-- | Define a recursive function with accumulation which finds the
-- | first longest word in a sentence.
-- | Make sure you keep track of the length of the currently longest word
-- | too, so you don't call `length` repeatedly on the same word.

getStr :: String -> String -> String
getStr s1 s2 | len1 < len2 = s2
             | otherwise   = s1
           where len1 = length s1
                 len2 = length s2

te511 :: String -> String
te511 xs = fun xss hs
  where xss = words xs
        hs  = head xss
        fun :: [String] -> String -> String
        fun []     s = s
        fun (x:xs) s = fun xs $ getStr s x

-- ** TE 5.1.2
--
-- | Define a recursive function with accumulation which takes a list of
-- | polynomial coefficients and a variable, and calculates the polynomial using
-- | Horner's method (https://en.wikipedia.org/wiki/Horner%27s_method ).
-- | The coefficients are in descending order of variable exponents, so a list
-- | representing the polynomial x^3 - 2x + 3 would be  as  [1, 0, -2, 3].

te512 :: Num a => [a] -> a -> a
te512 xs val = fun xs val 0
    where fun :: Num a => [a] -> a -> a -> a
          fun [] val acc     = acc
          fun (x:xs) val acc = fun xs val $ acc * val + x

-- ** TE 5.1.3
--
-- | Define a function which computes the population standard deviation of a list of
-- | numbers. To achieve this you need to compute the mean and variance of the list:
-- | do this using recursive functions with accumulation.

sqr :: Num a => a -> a
sqr x = x * x

mean :: Floating a => [a] -> a -> a -> a
mean [] len acc     = acc / len
mean (x:xs) len acc = mean xs len (acc + x)

f' :: Floating a => [a] -> a -> a -> a
f' [] _ acc      = acc
f' (x:xs) mi acc = f' xs mi $ acc + sqr (x - mi)

te513 :: Floating a => [a] -> a
te513 xs =  sqrt $ suma / len
   where len = realToFrac $ length xs
         mi = mean xs len 0
         suma = f' xs mi 0

-- ** TE 5.1.4
--
-- | An aspiring rollercoaster designer wants to test out his if his newest
-- | creation is safe to ride, and needs your help!
-- | Define a function which takes a list of pairs which describe a section of
-- | the track. The first element will be a String which will be either "even",
-- | "drop", "turn left" or "turn right". The second element will be a number.
-- |
-- | If it's a "drop", the car accelerates as if it was in free-fall (accelerating
-- | 9.81 m/s^2) and the number indicates the height of the drop in meters.
-- | The car maintains its current speed coming into the drop.
-- |
-- | If it's "even", the car decelerates by 0.5 m/s every meter it passes. The
-- | number indicates the length of the even segment. If the car decelerates to
-- | 0 km/s, the track is deemed unsafe as the passengers will become stuck!
-- |
-- | If it's either of the two "turn"s, the number indicates the radius of the
-- | turn in meters. The car will derail if it turns too tightly: it can only
-- | withstand centripetal acceleration of up to and including 5G. And if there
-- | are 3 or more alternating turns directly in a row, the passengers will become
-- | nauseous, which can be unsafe.
-- |
-- | The car starts moving at 20 km/h. The function must return a list indicating
-- | whether the rollercoaster is safe or not. If it is safe, it returns an empty list.
-- | If the rollercoaster is not safe, it returns a list with one element: the
-- | index of the segment of the track where it becomes unsafe.
-- | (Later on, you will learn a much more elegant way of representing a result
-- | which might contain a value, or might contain nothing at all, but this will
-- | do for now.)

te514 :: [(String, Double)] -> [Int]
te514 = undefined

-- ** TE 5.1.5 - EXTRA
--
-- | Define a recursive function with accumulation which computes the square root
-- | of a given number using Newton's method, with the given number of iterations.
-- | Use the halved original number as an initial guess for the method.
te515 :: (Ord a, Fractional a, Integral b) => a -> b -> a
te515 val iter = newton val iter 0 
      where newton _ 0 acc      = acc
            newton val iter acc = newton val (iter-1) (acc - (acc*acc-val)/(2*acc)) 
