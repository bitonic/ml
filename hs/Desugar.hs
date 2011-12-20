{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Desugar
       ( Decl (..)
       , Term (..)
       , Pattern (..)
       , Literal (..)
       , DataBody
       , TypeSig
       , desugar
       , prettyDesugar
       ) where

import Control.Arrow (first)
import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2)
import Data.Traversable (traverse)
import Text.PrettyPrint

import Fresh
import Lexer (Id)
import Parser ( Decl (..), TypeSig (..), Literal (..), Pattern (..), DataBody,
                pLiteral, pCases, pDecl
              )
import qualified Parser as P
import Fix

data Term = Var Id
          | Abs Id Term
          | App Term Term
          | Let Id Term Term
          | Fix Id Term
          | Literal (Literal Term)
          | Case Term [(Pattern Id, Term)]
          deriving (Show, Eq)

desugar :: [Decl P.Term] -> [Decl Term]
desugar decls = evalFresh (mapM dDecl decls) (0 :: Integer)

dDecl :: (MonadFresh c m, Applicative m, Show c) => Decl P.Term -> m (Decl Term)
dDecl (ValDecl v t) = liftM (ValDecl v) (dTerm t)
dDecl (DataDecl con tvs body) = return (DataDecl con tvs body)

freshVar :: (Show c, MonadFresh c m) => m Id
freshVar = liftM (('v' :) . show) fresh

depat :: (Show c, MonadFresh c m, Applicative m)
         => Fix Pattern -> P.Term -> (Id -> Term -> Term)
         -> ((Term -> Term) -> m Term) -> m Term
depat pt t f alt = case unRoll pt of
    VarPat v -> alt (f v)
    _ -> do
        v <- freshVar
        liftM (f v) (dTerm (P.Case (P.Var v) [(pt, t)]))

dTerm :: (Show c, MonadFresh c m, Applicative m) => P.Term -> m Term
dTerm (P.Abs pts t) = go pts
  where
    go [] = dTerm t
    go (pt : pts') = depat pt (P.Abs pts' t) Abs (`liftM` (go pts'))
dTerm (P.Let pt t1 t2) = do
    dt1 <- dTerm t1
    dt2 <- dTerm t2
    depat pt t2 (\v -> Let v dt1) (`liftM` (return dt2))
dTerm (P.Case term cases) = liftM2 Case (dTerm term)
                            (mapM dCase (map (first unRoll) cases))
  where
    dCase (VarPat v, t) = liftM (VarPat v,) (dTerm t)
    dCase (Pat con Nothing, t) = liftM (Pat con Nothing,) (dTerm t)
    dCase (Pat con (Just pt), t) = case unRoll pt of
        VarPat v -> liftM (Pat con (Just v),) (dTerm t)
        _ -> do
            v <- freshVar
            liftM (Pat con (Just v),) (dTerm (P.Case (P.Var v) [(pt, t)]))
    dCase (LitPat (TupleLit pts), t) = do
        (vs, t') <- dTupLit pts t
        liftM (LitPat (TupleLit vs),) (dTerm t')
    dCase (LitPat (IntLit i), t) = liftM (LitPat (IntLit i),) (dTerm t)
    dCase (LitPat (RealLit r), t) = liftM (LitPat (RealLit r),) (dTerm t)

    dTupLit [] t = return ([] :: [Id], t)
    dTupLit (pt : pts) t = do
        (vs, t') <- dTupLit pts t
        case unRoll pt of
            P.VarPat v -> return (v : vs, t')
            _ -> do
                v <- freshVar
                return (v : vs, P.Case (P.Var v) [(pt, t')])

dTerm (P.Var v) = return (Var v)
dTerm (P.Fix f t) = liftM (Fix f) (dTerm t)
dTerm (P.Literal lit) = liftM Literal (traverse dTerm lit)
dTerm (P.App l r) = liftM2 App (dTerm l) (dTerm r)

------ PRETTY PRINTING --------------------------------------------------------

prettyDesugar :: [Decl Term] -> String
prettyDesugar = render . vcat . map (pDecl pTerm)

pTerm :: Term -> Doc
pTerm (Var v) = text v
pTerm (Abs v t) = "\\" <> text v <+> "->" <+> pTerm t
pTerm (App t1 t2) = pTerm t1 <+> parensTerm t2
pTerm (Let v t1 t2) = sep [ "let" <+> text v <+> equals <+>
                            pTerm t1 <+> "in"
                          , pTerm t2
                          ]
pTerm (Fix f t) = "fix" <+> text f <+> "->" <+> pTerm t
pTerm (Literal lit) = pLiteral pTerm lit
pTerm (Case t cases) = ("case" <+> pTerm t <+> "of") $+$ nest 4 (pCases text pTerm cases)

parensTerm :: Term -> Doc
parensTerm t = case t of
    Abs _ _   -> parens d
    Let _ _ _ -> parens d
    Fix _ _   -> parens d
    App _ _   -> parens d
    _         -> d
  where
    d = pTerm t
