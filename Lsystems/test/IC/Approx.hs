{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module IC.Approx (ApproxEq(..), (~~>)) where

import Test.Tasty.HUnit (assertFailure, Assertion, HasCallStack)

import Control.Monad (unless)
import Data.List (sort)

import IC.Colour

epsilon :: Fractional a => a
epsilon = 1e-3

infix 4 ~=
class ApproxEq a where
  (~=) :: a -> a -> Bool

instance ApproxEq Int where (~=) = (==)
instance ApproxEq Bool where (~=) = (==)
instance ApproxEq Char where (~=) = (==)
instance ApproxEq Integer where (~=) = (==)
instance ApproxEq Word where (~=) = (==)
instance ApproxEq Colour where (~=) = (==)

instance ApproxEq Float where x ~= y = abs (y - x) < epsilon
instance ApproxEq Double where x ~= y = abs (y - x) < epsilon

-- List comparison that does not care about order
instance (Ord a, ApproxEq a) => ApproxEq [a] where
  xs ~= ys = length xs == length ys && and (zipWith (~=) (sort xs) (sort ys))

instance (ApproxEq a, ApproxEq b) => ApproxEq (a, b) where
  (x1, y1) ~= (x2, y2) = x1 ~= x2 && y1 ~= y2

instance (ApproxEq a, ApproxEq b, ApproxEq c) => ApproxEq (a, b, c) where
  (x1, y1, z1) ~= (x2, y2, z2) = x1 ~= x2 && y1 ~= y2 && z1 ~= z2

{-|
Asserts that the specified actual value is approximately equal to the
expected value (within some tolerance). The output message will contain
the prefix, the expected value, the actual value, and how much they are
allowed to vary.

If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
and only the expected and actual values are output.
-}
assertApproxEqual :: (HasCallStack, ApproxEq a, Show a)
                  => String -- ^ The message prefix
                  -> a      -- ^ The expected value
                  -> a      -- ^ The actual value
                  -> Assertion
assertApproxEqual preface expected actual =
  unless (expected ~= actual) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n")
           ++ "expected: " ++ show expected
           ++ "\n but got: " ++ show actual
           ++ "\n (floating values must be within: " ++ show epsilon ++ ")"

{-|
This function ensures that its first argument is roughly same as the second one.

This may mean that floating values are within a certain tolerance of each other,
or perhaps lists roughly the same elements (even if a different order).
-}
infix 1 ~~>
(~~>) :: (Show a, ApproxEq a, HasCallStack)
      => a -- ^ the actual value
      -> a -- ^ the expected value
      -> Assertion
actual ~~> expected = assertApproxEqual "" expected actual
