module Main where

import Data.List
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.Arbitrary.Haskell
import Text.Regex.Posix

moduleIsMain (H s) = s =~ "^module Main\\s" :: Bool

main = defaultMain $ testGroup "All tests" [
         testProperty "Only Main modules are generated" moduleIsMain
       ]
