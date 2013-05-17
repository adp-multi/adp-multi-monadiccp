module ADP.Tests.RGExample where

import ADP.Multi.All
                                
type RG_Algebra alphabet answer = (
  EPS -> answer,                              -- nil
  answer   -> answer -> answer,               -- left
  answer   -> answer -> answer -> answer,     -- pair
  answer   -> answer -> answer -> answer -> answer -> answer -> answer, -- knot
  answer   -> answer -> answer,               -- knot1
  answer   -> answer,                         -- knot2
  (alphabet, alphabet) -> answer,             -- basepair
  alphabet -> answer,                         -- base
  [answer] -> [answer]                        -- h
  )

data Start = Nil
           | Left' Start Start
           | Pair Start Start Start
           | Knot Start Start Start Start Start Start
           | Knot1 Start Start
           | Knot2 Start
           | BasePair (Char, Char)
           | Base Char
           deriving (Eq, Show)

enum :: RG_Algebra Char Start
enum = (\_->Nil,Left',Pair,Knot,Knot1,Knot2,BasePair,Base,id)