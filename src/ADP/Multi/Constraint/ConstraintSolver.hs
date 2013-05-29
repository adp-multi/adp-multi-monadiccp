{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-
Use monadiccp/OvertonFD as a finite-domain constraint solver to construct
subwords in a generic way.

TODO It is really slow. Maybe it is possible to "compile" the two inequality
     systems so that they can later be run faster.
     see http://www.cs.washington.edu/research/constraints/solvers/cp97.html
-}
module ADP.Multi.Constraint.ConstraintSolver (
        constructSubwords1,
        constructSubwords2
) where

import Control.Exception 
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.RangesHelper
import ADP.Multi.Constraint.MonadicCpHelper
import Control.CP.FD.Interface


constructSubwords1 :: SubwordConstructionAlgorithm Dim1
constructSubwords1 _ _ b | trace ("constructSubwords1 " ++ show b) False = undefined
constructSubwords1 f infos [i,j] =
        assert (i <= j) $
        let yieldSizeMap = buildYieldSizeMap infos
            symbolIDs = Map.keys yieldSizeMap
            rewritten = f symbolIDs
            parserCount = length infos
            remainingParsers = [parserCount,parserCount-1..1] `zip` infos
            rangeDesc = [(i,j,rewritten)]
        in trace ("f " ++ show symbolIDs ++ " = " ++ show rewritten) $
           assert (length rewritten == Map.size yieldSizeMap && all (`elem` rewritten) symbolIDs) $
           constructSubwordsRec yieldSizeMap remainingParsers rangeDesc

constructSubwords2 :: SubwordConstructionAlgorithm Dim2
constructSubwords2 _ _ b | trace ("constructSubwords2 " ++ show b) False = undefined
constructSubwords2 f infos [i,j,k,l] =
        assert (i <= j && j <= k && k <= l) $
        let yieldSizeMap = buildYieldSizeMap infos
            symbolIDs = Map.keys yieldSizeMap
            (left,right) = f symbolIDs
            parserCount = length infos
            remainingParsers = [parserCount,parserCount-1..1] `zip` infos
            rangeDescs = [(i,j,left),(k,l,right)]
        in trace ("f " ++ show symbolIDs ++ " = (" ++ show left ++ "," ++ show right ++ ")") $
           assert (length left + length right == Map.size yieldSizeMap && 
                   all (`elem` (left ++ right)) symbolIDs &&
                   not (null left) && not (null right)) $
           constructSubwordsRec yieldSizeMap remainingParsers rangeDescs



constructSubwordsRec :: YieldSizeMap -> [(Int,ParserInfo)] -> [RangeDesc] -> [SubwordTree]
constructSubwordsRec a b c | trace ("constructSubwordsRec " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
constructSubwordsRec _ [] [] = []
constructSubwordsRec yieldSizeMap ((current,ParserInfo1 {}):rest) rangeDescs =
        let symbolLoc = findSymbol1 current rangeDescs
            subwords = calcSubwords1 yieldSizeMap symbolLoc
        in trace ("calc subwords for dim1") $
           trace ("subwords: " ++ show subwords) $
           [ SubwordTree [i,j] restTrees |
             (i,j) <- subwords,
             let newDescs = constructNewRangeDescs1 rangeDescs symbolLoc (i,j),
             let restTrees = constructSubwordsRec yieldSizeMap rest newDescs
           ]
constructSubwordsRec yieldSizeMap ((current,ParserInfo2 {}):rest) rangeDescs =
        let symbolLocs = findSymbol2 current rangeDescs
            subwords = calcSubwords2 yieldSizeMap symbolLocs
        in trace ("calc subwords for dim2") $
           trace ("subwords: " ++ show subwords) $
           [ SubwordTree [i,j,k,l] restTrees |
             (i,j,k,l) <- subwords,
             let newDescs = constructNewRangeDescs2 rangeDescs symbolLocs (i,j,k,l),
             let restTrees = constructSubwordsRec yieldSizeMap rest newDescs
           ]
constructSubwordsRec _ [] r@(_:_) = error ("programming error " ++ show r)


calcSubwords2 :: YieldSizeMap -> (SymbolPos,SymbolPos) -> [Subword2]
calcSubwords2 a b | trace ("calcSubwords " ++ show a ++ " " ++ show b) False = undefined
calcSubwords2 yieldSizeMap (left@((i,j,r),sym1Idx),right@((_,_,r'),sym2Idx))
  | r == r' = calcSubwords2Dependent yieldSizeMap (i,j,r) sym1Idx sym2Idx
  | otherwise = [ (i',j',k',l') |
                  (i',j') <- calcSubwords1 yieldSizeMap left
                , (k',l') <- calcSubwords1 yieldSizeMap right
                ]

calcSubwords1 :: YieldSizeMap -> SymbolPos -> [Subword1]
calcSubwords1 _ b | trace ("calcSubwordsIndependent " ++ show b) False = undefined
calcSubwords1 yieldSizeMap pos@((i,j,_),_) =
        let (minY,maxY) = yieldSizeOf yieldSizeMap pos
            (minYLeft,maxYLeft) = combinedYieldSizeLeftOf yieldSizeMap pos
            (minYRight,maxYRight) = combinedYieldSizeRightOf yieldSizeMap pos
            model :: FDModel
            model = exists $ \col -> do
                  let rangeLen = fromIntegral (j-i)
                      [minY',minYLeft',minYRight'] = map fromIntegral [minY,minYLeft,minYRight]
                      [maxY',maxYLeft',maxYRight'] = map (maybe rangeLen fromIntegral) [maxY,maxYLeft,maxYRight]
                  [len1,len2,len3] <- colList col 3
                  xsum col @= rangeLen
                  len1 @>= minYLeft' 
                  len2 @>= minY'
                  len3 @>= minYRight'
                  len1 @<= maxYLeft'
                  len2 @<= maxY'
                  len3 @<= maxYRight'
                  rangeLen - maxYLeft'  @<= len2 + len3
                  rangeLen - maxYRight' @<= len1 + len2
                  rangeLen - maxY'      @<= len1 + len3
                  return col
        in map (\[len1,_,len3] -> (i+len1, j-len3)) $ solveModel model


calcSubwords2Dependent :: YieldSizeMap -> RangeDesc -> Int -> Int -> [Subword2]
calcSubwords2Dependent _ b c d | trace ("calcSubwordsDependent " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
calcSubwords2Dependent yieldSizeMap desc sym1Idx sym2Idx =
        let sym1Idx' = if sym1Idx < sym2Idx then sym1Idx else sym2Idx
            sym2Idx' = if sym1Idx < sym2Idx then sym2Idx else sym1Idx
            subs = doCalcSubwords2Dependent yieldSizeMap desc sym1Idx' sym2Idx'
        in if sym1Idx < sym2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]

doCalcSubwords2Dependent :: YieldSizeMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwords2Dependent yieldSizeMap desc@(i,j,_) sym1Idx sym2Idx =
        let (minY1,maxY1) = yieldSizeOf yieldSizeMap (desc,sym1Idx)
            (minY2,maxY2) = yieldSizeOf yieldSizeMap (desc,sym2Idx)
            (minYLeft1,maxYLeft1) = combinedYieldSizeLeftOf yieldSizeMap (desc,sym1Idx)
            (minYRight1,maxYRight1) = combinedYieldSizeRightOf yieldSizeMap (desc,sym1Idx)
            (minYRight2,maxYRight2) = combinedYieldSizeRightOf yieldSizeMap (desc,sym2Idx)
            minYBetween = minYRight1 - minYRight2 - minY2
            maxYBetween | sym1Idx + 1 == sym2Idx = Just 0
                        | isNothing maxYRight1 = Nothing
                        | otherwise = Just $ fromJust maxYRight1 - fromJust maxYRight2 - fromJust maxY2
            model :: FDModel
            model = exists $ \col -> do
                  let rangeLen = fromIntegral (j-i)
                      [minYLeft1',minY1',minYBetween',minY2',minYRight2'] =
                          map fromIntegral [minYLeft1,minY1,minYBetween,minY2,minYRight2]
                      [maxYLeft1',maxY1',maxYBetween',maxY2',maxYRight2'] =
                          map (maybe rangeLen fromIntegral) [maxYLeft1,maxY1,maxYBetween,maxY2,maxYRight2]

                  [lenLeft1,len1,lenBetween,len2,lenRight2] <- colList col 5
                  xsum col @= rangeLen
                  lenLeft1   @>= minYLeft1'
                  len1       @>= minY1'
                  lenBetween @>= minYBetween'
                  len2       @>= minY2'
                  lenRight2  @>= minYRight2'
                  lenLeft1   @<= maxYLeft1'
                  len1       @<= maxY1'
                  lenBetween @<= maxYBetween'
                  len2       @<= maxY2'
                  lenRight2  @<= maxYRight2'
                  rangeLen - maxYLeft1'   @<= len1 + lenBetween + len2 + lenRight2
                  rangeLen - maxY1'       @<= lenLeft1 + lenBetween + len2 + lenRight2
                  rangeLen - maxYBetween' @<= lenLeft1 + len1 + len2 + lenRight2
                  rangeLen - maxY2'       @<= lenLeft1 + len1 + lenBetween + lenRight2
                  rangeLen - maxYRight2'  @<= lenLeft1 + len1 + lenBetween + len2
                  return col
        in map (\ [lenLeft1,len1,_,len2,lenRight2] ->
                  ( i + lenLeft1
                  , i + lenLeft1 + len1
                  , j - lenRight2 - len2
                  , j - lenRight2
                  )
               ) $ solveModel model
