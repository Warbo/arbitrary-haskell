module Main where

import qualified Impure
import qualified Pure

import Test.Tasty (defaultMain, testGroup)

main = do ip <- Impure.tests
          defaultMain $ testGroup "All tests" [Pure.tests, ip]
