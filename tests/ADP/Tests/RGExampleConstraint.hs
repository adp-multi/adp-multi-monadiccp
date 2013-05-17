module ADP.Tests.RGExampleConstraint where

import ADP.Multi.All
import ADP.Multi.Constraint.All

import ADP.Tests.RGExample
   
rgknot :: RG_Algebra Char answer -> String -> [answer]
rgknot algebra inp =
  let  
  (nil,left,pair,knot,knot1,knot2,basepair,base,h) = algebra
   
  rewritePair, rewriteKnot :: Dim1
   
  rewritePair [p1,p2,s1,s2] = [p1,s1,p2,s2]
  rewriteKnot [k11,k12,k21,k22,s1,s2,s3,s4] = [k11,s1,k21,s2,k12,s3,k22,s4]
  
  s = tabulated1 $
      yieldSize1 (0,Nothing) $
      nil  <<< EPS >>> id1 |||
      left <<< b ~~~ s >>> id1 |||
      pair <<< p ~~~ s ~~~ s >>> rewritePair |||
      knot <<< k ~~~ k ~~~ s ~~~ s ~~~ s ~~~ s >>> rewriteKnot
      ... h
  
  b = tabulated1 $
      base <<< 'a' >>> id1 |||
      base <<< 'u' >>> id1 |||
      base <<< 'c' >>> id1 |||
      base <<< 'g' >>> id1
      ... h
  
  p = tabulated2 $
      basepair <<< ('a', 'u') >>> id2 |||
      basepair <<< ('u', 'a') >>> id2 |||
      basepair <<< ('c', 'g') >>> id2 |||
      basepair <<< ('g', 'c') >>> id2 |||
      basepair <<< ('g', 'u') >>> id2 |||
      basepair <<< ('u', 'g') >>> id2
      ... h
  
  rewriteKnot1 :: Dim2
  rewriteKnot1 [p1,p2,k1,k2] = ([k1,p1],[p2,k2])
  
  k = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      knot1 <<< p ~~~ k >>> rewriteKnot1 |||
      knot2 <<< p >>> id2
      ... h
      
  z = mk inp
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in axiom z s