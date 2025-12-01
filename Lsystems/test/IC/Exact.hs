module IC.Exact where

import Test.Tasty.HUnit ((@?=), Assertion, HasCallStack)

{-|
This function ensures that its first argument is the same as the second one.
-}
infix 1 -->
(-->) :: (Show a, Eq a, HasCallStack)
      => a -- ^ the actual value
      -> a -- ^ the expected value
      -> Assertion
(-->) = (@?=)
