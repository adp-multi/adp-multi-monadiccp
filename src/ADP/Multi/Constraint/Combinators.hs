{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
Use this set of combinators instead of ADP.Multi.Combinators to
use a constraint solver for constructing the indices ranges.

Note: This is experimental and slow.
-} 
module ADP.Multi.Constraint.Combinators where

import ADP.Multi.Combinators
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.Model
import ADP.Multi.Constraint.ConstraintSolver

instance Rewritable Dim1 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize1 f infos, rewrite constructRanges1 (infos,p) f)
    
instance Rewritable Dim2 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize2 f infos, rewrite constructRanges2 (infos,p) f)