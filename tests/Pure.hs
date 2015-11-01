module Pure where

import Data.List
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.Arbitrary.Haskell
import Test.Arbitrary.Internal.Cabal

requiredSectionsUnique rs = length unique == length rs'
  where (rs', _) = dropDupes (rs++rs) []
        unique   = nub (map getName (rs++rs))

optionalSectionsUnique os = length unique == length os'
  where (_, os') = dropDupes [] (os++os)
        unique   = nub (map getName (os++os))

dedupeDecreasesLength rs os = length rs' + length os' <= length rs + length os
  where (rs', os') = dropDupes (rs++rs) (os++os)

dedupedSectionsUnique rs os = nameCount == sectionCount
  where (rs', os')   = dropDupes (rs++rs) (os++os)
        nameCount    = length (nub (map getName rs' ++ map getName os'))
        sectionCount = length rs' + length os'

cabalSectionsUnique p = length (sections p) == length unique
  where unique = nub (map getName (sections p))

cabalTests = testGroup "Cabal tests" [
    testProperty "Deduping decreases length" dedupeDecreasesLength
  , testProperty "Deduped Required sections are unique" requiredSectionsUnique
  , testProperty "Deduped Optional sections are unique" optionalSectionsUnique
  , testProperty "Section names are unique" dedupedSectionsUnique
  , testProperty "Cabal section names are unique" cabalSectionsUnique
  ]

tests = testGroup "Pure tests" [cabalTests]
