module LabeledAst where

import qualified Ast as Ast
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set, union)
import Control.Monad.State
import Control.Applicative ((<*>))

type Label = Integer

type Context = Map.Map String LAst

data LAst = Var String Label
          | Function String LAst Label
          | Application LAst LAst Label
          | IfExpr LAst LAst LAst Label
          | LetExpr [(String, LAst)] LAst Label
          | LetRec  [(String, LAst)] LAst Label
          | Bool Bool Label
          | String String Label
          | Unit Label
          | Number Integer Label
          | BinaryExpr String LAst LAst Label
          | Closure LAst Context Label
          | List [LAst] Label
          | Car LAst Label
          | Cdr LAst Label
          | Cons LAst LAst Label
          | IsNil LAst Label
          deriving (Eq, Ord)

convert :: Ast.Ast -> (LAst, Integer)
convert old = let (program, l) = runState (convertM old) 1
              in (program, l - 1)

convertM :: Ast.Ast -> State Label LAst
convertM (Ast.Var v) = do l <- freshLabel
                          return $ Var v l
convertM (Ast.Function arg body) = do body' <- convertM body
                                      l <- freshLabel
                                      return $ Function arg body' l
convertM (Ast.Application e1 e2) = do e1' <- convertM e1
                                      e2' <- convertM e2
                                      l <- freshLabel
                                      return $ Application e1' e2' l
convertM (Ast.IfExpr cond e1 e2) = do cond' <- convertM cond
                                      e1' <- convertM e1
                                      e2' <- convertM e2
                                      l <- freshLabel
                                      return $ IfExpr cond' e1' e2' l
convertM (Ast.LetRec binds body) =
  do let decls = map fst binds
         inits = map snd binds
     inits' <- mapM convertM inits
     body' <- convertM body
     l <- freshLabel
     return $ LetRec (zip decls inits') body' l
     
convertM (Ast.LetExpr binds body) =
  do let decls = map fst binds
         inits = map snd binds
     inits' <- mapM convertM inits
     body' <- convertM body
     l <- freshLabel
     return $ LetExpr (zip decls inits') body' l
     
convertM (Ast.BinaryExpr op e1 e2) = do e1' <- convertM e1
                                        e2' <- convertM e2
                                        l <- freshLabel
                                        return $ BinaryExpr op e1' e2' l
convertM (Ast.List as) = do
  as' <- mapM convertM as
  l <- freshLabel
  return $ List as' l

convertM (Ast.Car lst) = do lst' <- convertM lst
                            l <- freshLabel
                            return $ Car lst' l

convertM (Ast.Cdr lst) = do lst' <- convertM lst
                            l <- freshLabel
                            return $ Cdr lst' l
convertM (Ast.Cons e lst) = do e' <- convertM e
                               lst' <- convertM lst
                               l <- freshLabel
                               return $ Cons e' lst' l
convertM (Ast.IsNil lst) = do lst' <- convertM lst
                              l <- freshLabel
                              return $ IsNil lst' l
convertM (Ast.Bool b) = do l <- freshLabel
                           return $ Bool b l
convertM (Ast.String s) = do l <- freshLabel
                             return $ String s l
convertM (Ast.Number n) = do l <- freshLabel
                             return $ Number n l
convertM Ast.Unit = do l <- freshLabel
                       return $ Unit l

allVar :: LAst -> Set String
allVar (Function x body _) = Set.singleton x `union` allVar body
allVar (LetRec binds body _) = Set.fromList (map fst binds) `union`
                               Set.unions (map allVar (map snd binds))`union`
                               allVar body
allVar (IfExpr cond e1 e2 _) = allVar cond `union` allVar e1 `union`
                               allVar e2
allVar (BinaryExpr _ e1 e2 _) = allVar e1 `union` allVar e2
allVar (Application e1 e2 _) = allVar e1 `union` allVar e2
allVar _ = Set.empty

allFunction :: LAst -> Set LAst
allFunction f@(Function _ body _) = Set.singleton f `union` allFunction body
allFunction (LetRec binds body _) = let inits = map snd binds
                                        funsList = map allFunction inits
                                    in allFunction body `union` Set.unions funsList
allFunction (IfExpr cond e1 e2 _) = allFunction cond `union`
                                    allFunction e1 `union`
                                    allFunction e2
allFunction (BinaryExpr _ e1 e2 _) = allFunction e1 `union`
                                     allFunction e2
allFunction (Application e1 e2 _) = allFunction e1 `union`
                                    allFunction e2
allFunction _ = Set.empty

labelOf :: LAst -> Label
labelOf (Var _ l) = l
labelOf (Function _ _ l) = l
labelOf (Application _ _ l) = l
labelOf (IfExpr _ _ _ l) = l
labelOf (LetRec _ _ l) = l
labelOf (BinaryExpr _ _ _ l) = l
labelOf (Number _ l) = l
labelOf (Bool _ l) = l
labelOf (String _ l) = l
labelOf (Unit l) = l
labelOf _ = error "No Label"

freshLabel = do l <- get
                put $ l + 1
                return l

addLable l = " @<" ++ show l ++ "> "


instance Show LAst where
  show (Var v l) = v ++ addLable l
  show (Function v body l) = "function (" ++ v ++ ") {" ++
                             show body ++ "}" ++ addLable l
  show (IfExpr cond e1 e2 l) = "if (" ++ show cond ++ ") {" ++
                               show e1 ++ "} else {" ++ show e2 ++
                               "}" ++ addLable l
  show (LetRec binds body l) =
    let vs = unwords $ map (\(v,i) -> "var " ++ v ++ " = " ++ show i ++ ";\n" ) binds
    in vs ++ show body ++ addLable l
  show (Bool False l) = "false" ++ addLable l
  show (Bool True l) = "true" ++ addLable l
  show (String s l) = s ++ addLable l
  show (Number n l) = show n ++ addLable l
  show (Unit l) = "()" ++ addLable l
  show (BinaryExpr op e1 e2 l) = show e1 ++ op ++ show e2 ++ addLable l
  show (Application e1 e2 l) = show e1 ++ "(" ++ show e2 ++ ")" ++ addLable l 
  show _ = "&&&&"

