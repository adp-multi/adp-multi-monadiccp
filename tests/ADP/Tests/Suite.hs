{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Monoid (mempty)

import Test.HUnit
import Test.QuickCheck

import Data.Char (toLower)

import qualified ADP.Tests.RGExample as RG
import qualified ADP.Tests.RGExampleExplicit as RGE
import qualified ADP.Tests.RGExampleConstraint as RGC

main :: IO ()
main = defaultMainWithOpts
            [
                    testProperty "subword consistency" prop_constraint
            ]
       mempty {
            ropt_test_options = Just mempty {
                topt_maximum_generated_tests = Just 20
            }
       }


-- checks if both subword construction algorithms produce the same subwords
-- by testing whether a grammar produces the same terms in both cases
prop_constraint (RNAString w) =
    let resultsExplicit = RGE.rgknot RG.enum w
        resultsConstraint = RGC.rgknot RG.enum w
    in -- the constraint solver returns subwords in the same order
       -- as the explicit algorithm, therefore a simple equality test is enough 
       resultsExplicit == resultsConstraint

          
                   
newtype RNAString = RNAString String deriving (Show)
instance Arbitrary RNAString where
    arbitrary = genAlphabetString RNAString "agcu"

-- returns a small test string consisting of letters from an alphabet
genAlphabetString typ alph =
    sized $ \n ->
    do s <- mapM (\_ -> elements alph) [0..round (sqrt (fromIntegral n))]
       return $ typ s
