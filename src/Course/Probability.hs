{-# LANGUAGE TupleSections #-}

module Course.Probability where

import Prelude hiding (elem)
import           Text.Printf (printf)
import           Control.Applicative (liftA2)
import qualified Data.Map as M


-- Taken from https://dennybritz.com/posts/probability-monads-from-scratch/
-- (Has the answers, if you're interested)

-- | We use a Double as the representation for probabilities
type Prob = Double

-- | An event maps an outcome to a truth value
type Event a = a -> Bool

-- | A distribution is represented as a list of possible values
-- and their probabilities
newtype Dist a = Dist [(a, Prob)]

-- | Helper function to access the inner list wrapped by the distribution
unpackDist :: Dist a -> [(a, Prob)]
unpackDist (Dist xs) = xs

-- | Combines outcomes that occur multiple time
squishD :: (Ord a) => Dist a -> Dist a
squishD = Dist . M.toList . M.fromListWith (+) . unpackDist

instance (Show a, Ord a) => Show (Dist a) where
  show d = concatMap showRow $ (unpackDist . squishD) d
    where
      showRow (elem, prob) = padded elem ++ " | " ++ printf "%.4f" prob ++ "\n"
      padded elem = replicate (maxElemLen - (length . show) elem) ' ' ++ show elem
      maxElemLen = maximum $ map (length . show . fst) (unpackDist d)

----------------------------------------------------------

-- | Sum all probilities in the given list
-- Hint, use `snd` to access the second element of a tuple
sumP :: [(a, Prob)] -> Prob
sumP = error "todo: Course.Probability#sumP"

-- | Normalize the probabilities to 1.0
normP :: [(a, Prob)] -> [(a, Prob)]
normP = error "todo: Course.Probability#normP"

-- | Create a uniform distribution
uniform :: [a] -> Dist a
uniform = error "todo: Course.Probability#uniform"

-- | Evaluate the probability for the given event
evalD :: Event a -> Dist a -> Prob
evalD = error "todo: Course.Probability#evalD"


-- A 6-sided die
-- λ> die 6
-- 1 | 0.1667
-- 2 | 0.1667
-- 3 | 0.1667
-- 4 | 0.1667
-- 5 | 0.1667
-- 6 | 0.1667
-- | A fair n-sided die
die :: Int -> Dist Int
die = error "todo: Course.Probability#uniform"

-- λ> coin 0.3 True False
-- False | 0.7000
--  True | 0.3000
-- | A coin that lands on x with probability f and y with probability 1-f
coin :: Prob -> a -> a -> Dist a
coin = error "todo: Course.Probability#coin"

-- Probability of rolling an even number with a 5-sided die
-- λ> evalD even $ die 5
-- 0.4

instance Functor Dist where
  fmap = error "todo: Course.Probability#fmap"

sample :: Dist (Integer, Integer)
sample =
  Dist
    [ ((0, 0), 0.1),
      ((0, 1), 0.2),
      ((1, 0), 0.3),
      ((1, 1), 0.4)
    ]

-- λ> sample
-- (0,0) | 0.1000
-- (0,1) | 0.2000
-- (1,0) | 0.3000
-- (1,1) | 0.4000

-- -- Map the tuple to the first value (sum over the second)
-- λ> fst <$> sample
-- 0 | 0.3000
-- 1 | 0.7000

-- -- Map the tuple to the second value (sum over the first)
-- λ>  snd <$> sample
-- 0 | 0.4000
-- 1 | 0.6000

instance Applicative Dist where
  -- pure :: a -> Dist a
  pure = error "todo: Course.Probability#pure"

  -- (<*>) :: Dist (a -> b) -> Dist a -> Dist b
  (Dist fs) <*> (Dist xs) = error "todo: Course.Probability#(<*>)"

-- --- Distribution of rolling a 5-sided die and then rolling a 4-sided die
-- λ> (,) <$> (die 5) <*> (die 4)
-- (1,1) | 0.0500
-- (1,2) | 0.0500
-- (1,3) | 0.0500
-- (1,4) | 0.0500
-- (2,1) | 0.0500
-- (2,2) | 0.0500
-- (2,3) | 0.0500
-- (2,4) | 0.0500
-- (3,1) | 0.0500
-- (3,2) | 0.0500
-- (3,3) | 0.0500
-- (3,4) | 0.0500
-- (4,1) | 0.0500
-- (4,2) | 0.0500
-- (4,3) | 0.0500
-- (4,4) | 0.0500
-- (5,1) | 0.0500
-- (5,2) | 0.0500
-- (5,3) | 0.0500
-- (5,4) | 0.0500

-- λ> liftA2 (+) (die 6) (die 6)
--  2 | 0.0278
--  3 | 0.0556
--  4 | 0.0833
--  5 | 0.1111
--  6 | 0.1389
--  7 | 0.1667
--  8 | 0.1389
--  9 | 0.1111
-- 10 | 0.0833
-- 11 | 0.0556
-- 12 | 0.0278

-- | Binomial distribution with n experiments and success probability p
binom :: Int -> Prob -> Dist Int
binom n p = foldl1 (\x y -> squishD (liftA2 (+) x y)) $ replicate n (coin p 1 0)

-- λ> binom 10 0.3
--  0 | 0.0282
--  1 | 0.1211
--  2 | 0.2335
--  3 | 0.2668
--  4 | 0.2001
--  5 | 0.1029
--  6 | 0.0368
--  7 | 0.0090
--  8 | 0.0014
--  9 | 0.0001
-- 10 | 0.0000

instance Monad Dist where
  -- (>>=) :: Dist a -> (a -> Dist b) -> Dist b
  (Dist xs) >>= f = error "todo: Course.Probability#(>>=)"

-- Condition a distribution on an event
condD :: (a -> Bool) -> Dist a -> Dist a
condD f (Dist xs) = Dist . normP $ filter (f . fst) xs

-- λ> evalD ((==2) . fst) $ condD ((<= 5) . uncurry (+)) $ liftA2 (,) (die 6) (die 6)
-- 0.29999999999

-- Implement a medical test with a 1% chance of having a disease,
-- and 95% chance of testing positive if you have the disease
bayesMedicalTest :: Prob
bayesMedicalTest = error "todo: Course.Probability#bayesMedicalTest"

-- λ> bayesMedicalTest
-- 0.16101694915254236
