{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ADP.Tests.MonadicCpTest where

import Control.CP.FD.Interface
import ADP.Multi.Constraint.MonadicCpHelper

main :: IO ()
main = print $ solveModel model

model :: FDModel
model = exists $ \col -> do
  [x1,x2] <- colList col 2
  allin col (cte 0,cte 8)
  x1 + x2 @= 8
  x1 @>= 1
  x2 @>= 2
  x1 @<= 10
  x2 @<= 12
  -2 @<= x2
  -4 @<= x1
  return col