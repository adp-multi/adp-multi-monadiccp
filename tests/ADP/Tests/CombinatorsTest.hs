{-
An alternative to ADP.Multi.Constraint.All if both subword
construction algorithms should be used together in a project,
e.g. for benchmarking.

Using ADP.Multi.Rewriting.All and ADP.Multi.Constraint.All together
would lead to overlapping instances. As a remedy, this module can be
used and >>> has to be replaced by >>>| and >>>|| for dimension 1 and 2.

Phantom types could have been used to prevent overlapping instances,
but then Dim1 and Dim2 would have to be made newtype's instead of
type synonyms which would lead to something like:

rewritePair = Dim1 $ \[p1,p2,s1,s2] -> [p1,s1,p2,s2]

for rewriting functions, and

pair <<< p ~~~ s ~~~ s >>> (rewritePair :: Dim1 Explicit) |||

or alternatively

pair <<< p ~~~ s ~~~ s >>> (rewritePair :: Dim1 ConstraintSolver) |||

for productions. As the case of using both algorithms together is
not the standard use case and the syntax would become very noise,
phantom types were not used.
-}
module ADP.Tests.CombinatorsTest (
    (>>>|), (>>>||),
    id1, id2, Dim1, Dim2
) where

import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Combinators (rewrite)
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize

import ADP.Multi.Constraint.ConstraintSolver

infix 6 >>>|
(>>>|) :: ([ParserInfo], [SubwordTree] -> Parser a b) -> Dim1 -> RichParser a b
(>>>|) (infos,p) f = 
      (determineYieldSize1 f infos, rewrite constructSubwords1 (infos,p) f)

infix 6 >>>||
(>>>||) :: ([ParserInfo], [SubwordTree] -> Parser a b) -> Dim2 -> RichParser a b   
(>>>||) (infos,p) f = 
      (determineYieldSize2 f infos, rewrite constructSubwords2 (infos,p) f)