import Criterion.Main
import Criterion.Helpers

import qualified ADP.Tests.RGExample as RG
import qualified ADP.Tests.RGExampleExplicit as RGE
import qualified ADP.Tests.RGExampleConstraint as RGC
     
-- run with -s 10 -o report.html
main :: IO ()
main = defaultMain
          [
              bgroup "explicit" (benchArray (RGE.rgknot RG.maxBasepairs) inputs),
              bgroup "constraint solver" (benchArray (RGC.rgknot RG.maxBasepairs) inputs)
          ]
     where
        longInp = "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuag"
        infiniteInp = cycle longInp
        inputs = [ (show i, take i infiniteInp) | i <- [5,10..15] ]