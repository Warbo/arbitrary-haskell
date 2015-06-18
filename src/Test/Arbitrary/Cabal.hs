module Test.Arbitrary.Cabal where

import System.Directory
import Test.QuickCheck

-- | Alphabetic strings
newtype AlphaName = AN String deriving (Show)

alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

instance Arbitrary AlphaName where
  arbitrary = fmap AN (listOf1 (elements alpha))

-- | A Cabal project
data Project = P { name :: String
                 }
instance Show Project where
  show p = "name: " ++ name p

instance Arbitrary Project where
  arbitrary = do AN n <- arbitrary
                 return $ P { name = n }

-- | Create a Cabal project inside the given directory, returning the path to
--   the project
makeProject :: FilePath -> Project -> IO FilePath
makeProject d p = let path = d ++ "/" ++ name p in do
  createDirectoryIfMissing True path
  --writeFile (path ++ ("/" ++ pName p ++ ".cabal")) (pCabal p)
  --createDirectoryIfMissing True (path ++ "/src")
  return path
