module ControlFlow where

import LabeledAst
import Data.Set (Set, union, singleton, isSubsetOf, member)
import qualified Data.Set as Set
import Data.Map (Map, findWithDefault, (!))
import qualified Data.Map as Map
import Data.List (partition)
import Ast (Ast)

data Abstract = Cache Label Context
              | Envir String Context
              deriving (Eq, Ord, Show)

data Constraint = Concrete LAst Abstract
                | Subset Abstract Abstract
                | Conditional LAst Abstract Abstract Abstract
                | Incomplete Abstract Abstract Abstract Context CachedCalls
                deriving (Eq, Ord, Show)

type CallAbstraction = (ContextEnvironment, Context, Label)
type CachedCalls = Set CallAbstraction

type AbstractValue = [(LAst, ContextEnvironment)]

k :: Int
k = 1

controlFlow :: Ast -> (Map Abstract (Set LAst), Map Abstract (Set LAst))
controlFlow ast = let (lAst, _) = convert ast
                      result = solve $ constraints lAst
                  in Map.partitionWithKey (\key _ -> isCache key) result               

constraints :: LAst -> Set Constraint
constraints ast = genConstraints Set.empty Map.empty [] ast

genConstraints :: CachedCalls -> ContextEnvironment -> Context -> LAst -> Set Constraint
genConstraints _ ce current (Var x l) =
  let ctx = ce ! x
  in singleton $ Subset (Envir x ctx) (Cache l current)
genConstraints _ ce current f@(Function _ _ l) =
  let freeVariables = fv f
      ce0 = Map.filterWithKey (\key _ -> key `member` freeVariables) ce
  in singleton $ Concrete (Closure f ce0) (Cache l current)
genConstraints ca ce current (IfExpr cond t1 t2 l) =
  genConstraints ca ce current cond `union`
  genConstraints ca ce current t1 `union`
  genConstraints ca ce current t2 `union`
  Set.fromList [Subset (Cache l1 current) (Cache l current), Subset (Cache l2 current) (Cache l current)]
  where l1 = labelOf t1
        l2 = labelOf t2
genConstraints ca ce current (LetRec binds body l) =
  let vars = map fst binds
      inits = map snd binds
      newEnv = Map.fromList $ zip vars (repeat current)
      ce' = Map.union newEnv ce
      l2 = labelOf body
  in Set.unions (map (genConstraints ca ce current) inits) `union`
     genConstraints ca ce' current body `union`
     singleton (Subset (Cache l2 current) (Cache l current)) `union`
     Set.unions (map (\(x, i) ->
                         singleton $ Subset (Cache (labelOf i) current) (Envir x current)) binds)
genConstraints cachedCalls ce current (Application t1 t2 l) =
  if ca `member` cachedCalls
  then Set.empty
  else genConstraints cachedCalls ce current t1 `union`
       genConstraints cachedCalls ce current t2 `union`
       singleton (Incomplete (Cache l1 current) (Cache l2 current) (Cache l current) next newCache)
  where ca = (ce, current, l)
        newCache = Set.insert ca cachedCalls
        next = take k $ l : current
        l1 = labelOf t1
        l2 = labelOf t2
genConstraints ca ce current (BinaryExpr _ t1 t2 _) =
  genConstraints ca ce current t1 `union` genConstraints ca ce current t2
genConstraints _ _ _ _ = Set.empty
      
solve :: Set Constraint -> Map Abstract (Set LAst)
solve consts =
  let (concretes, subCondiIncom) = partition isConcrete $ Set.toList consts
      (d, w) = buildData Map.empty [] concretes
      edgeArray :: Map Abstract [Constraint]
      (edgeArray, dataArray, workList) = buildEdge Map.empty d w subCondiIncom
  in iteration workList edgeArray dataArray

iteration :: [Abstract] -> Map Abstract [Constraint] -> Map Abstract (Set LAst) -> Map Abstract (Set LAst)
iteration [] _ d = d
iteration (q : w) e d = let (newEdgeArray, newDataArray, w') = buildResult (findE q e) Set.empty e d w
                        in iteration w' newEdgeArray newDataArray
buildEdge :: Map Abstract [Constraint] ->  Map Abstract (Set LAst) -> [Abstract] -> [Constraint] -> (Map Abstract [Constraint], Map Abstract (Set LAst), [Abstract])
buildEdge es ds w [] = (es, ds, w)
buildEdge es ds w (cc@(Subset p1 p2) : cs) =
  let (e', d', w') = buildEdge es ds w cs
      (d'', w'') = add p2 (findD p1 d') d' w'
  in (Map.insert p1 (cc : findE p1 e') e', d'', w'')
buildEdge es ds w (cc@(Conditional _ p p1 _) : cs) =
  let (remain, d', w') = buildEdge es ds w  cs
      remain' =  Map.insert p (cc : findE p remain) remain
  in (Map.insert p1 (cc : findE p1 remain') remain', d', w')
buildEdge es ds w (cc@(Incomplete p1 _ _ _ _) : cs) =
  let (remain, d', w') = buildEdge es ds w cs
  in (Map.insert p1 (cc : findE p1 remain) remain, d', w')

buildData ::  Map Abstract (Set LAst) -> [Abstract] -> [Constraint]  ->  (Map Abstract (Set LAst), [Abstract])
buildData d w [] = (d, w)
buildData d w (Concrete t p : cs) =
  let (d', w') = add p (singleton t) d w
  in buildData d' w' cs

buildResult :: [Constraint] -> Set LAst -> Map Abstract [Constraint] -> Map Abstract (Set LAst) -> [Abstract] -> (Map Abstract [Constraint], Map Abstract (Set LAst), [Abstract])    
buildResult [] _ e d w = (e, d, w)
buildResult (Subset p1 p2 : cs) processed e d w = 
  let (d', w') = add p2 (findD p1 d) d w 
  in buildResult cs processed e d' w'
buildResult (Conditional t p p1 p2 : cs) processed e d w =
  if t `member` findD p d
  then let (d', w') = add p2 (findD p1 d) d w
       in buildResult cs processed e d' w'
  else buildResult cs processed e d w
buildResult (Incomplete p1 p2 p env cachedCalls: cs) processed e d w =
  let dOfP1 = findD p1 d
      closures = Set.toList $ Set.filter isClosure dOfP1
      help t@(Closure (Function x body _) ce0) =
        if not $ t `member` processed
        then (singleton (Subset p2 (Envir x env)) `union`
              singleton (Subset (Cache l0 env) p) `union`
              genConstraints cachedCalls ce0' env body, singleton t)
        else (Set.empty, processed)
        where l0 = labelOf body
              ce0' = Map.insert x env ce0
      tmp = map help closures
      processed' = Set.unions $ processed : map snd tmp
      newConstraints = Set.unions $ map fst tmp
      (concretes, subCondiIncom) = partition isConcrete $ Set.toList newConstraints
      (tmpD, tmpW) = buildData d w concretes
      (e', d', w') = buildEdge e tmpD tmpW subCondiIncom
  in buildResult cs processed' e' d' w'

add :: Abstract -> Set LAst -> Map Abstract (Set LAst) -> [Abstract] -> (Map Abstract (Set LAst), [Abstract])
add q d da w =
  let dq = findD q da
  in if not $ d `isSubsetOf` dq
     then (Map.insert q (dq `union` d) da, q : w)
     else (da, w)

findE :: Abstract -> Map Abstract [Constraint] -> [Constraint]
findE =  findWithDefault []

findD :: Abstract ->  Map Abstract (Set LAst) ->  (Set LAst)
findD = findWithDefault Set.empty

isConcrete :: Constraint -> Bool
isConcrete (Concrete _ _) = True
isConcrete _ = False

isCache :: Abstract -> Bool
isCache (Cache _ _) = True
isCache _ = False

isClosure :: LAst -> Bool
isClosure (Closure _ _) = True
isClosure _ = False
