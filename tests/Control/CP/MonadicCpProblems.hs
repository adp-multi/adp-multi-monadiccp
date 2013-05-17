{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.CP.MonadicCpProblems where

import Control.CP.FD.Interface
import ADP.Multi.Constraint.MonadicCpHelper

main :: IO ()
main = print $ solveModel model


-- throws Exception: Cannot process EGBoolValue BoolConst True
model :: FDModel
model = exists $ \col -> do
  [len1,len2] <- colList col 2
  xsum col @= 2
  len1 @>= 0
  len2 @>= 1
  2 @<= 1 -- this causes the exception 
  return col

-- throws Exception: Cannot process EGLess True
model2 :: FDModel
model2 = exists $ \col -> do
  [x1,x2,x3,x4] <- colList col 4
  allin col (cte 0,cte 5)
  x1 @<= x2
  x3 @>= x4
  x3 @>= x2 @|| x4 @<= x1  -- this causes the exception 
  return col
