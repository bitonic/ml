{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Pretty
       ( prettyML
       , prettyDesugar
       , pType
       , pParensType

       , prettyScheme
       , prettyType
       , prettyAssump
       -- , prettySubst
       , pScheme
       , pKind
       , prettyKind
       ) where

import Text.PrettyPrint
import qualified Data.Map as Map
import Data.List (intersperse)

import Syntax
import TI.TypesTypes

-------------------------------------------------------------------------------

prettyML :: [Decl FullTerm] -> String
prettyML = render . vcat . map (\d -> pDecl (hsep . (map pPattern)) pPattern d)

pVar :: Var -> Doc
pVar = text . unVar

pCon :: Con -> Doc
pCon = text . unCon

prettyDesugar :: [Decl DTerm] -> String
prettyDesugar = render . vcat . map (pDecl pVar pVar)

pTerm :: (fn -> Doc) -> (lt -> Doc) -> Term fn lt -> Doc
pTerm f l (App (App (Con (ConN "(,)")) t1) t2) =
    "(" <> pTerm f l t1 <> "," <+> pTerm f l t2 <> ")"
pTerm f l (App (App (App (Con (ConN "(,,)")) t1) t2) t3) =
    "(" <> pTerm f l t1 <> "," <+> pTerm f l t2 <> "," <+> pTerm f l t3 <> ")"
pTerm _ _ (Var v) = text (unVar v)
pTerm _ _ (Con c) = text (unCon c)
pTerm f l (Abs pts t) = "\\" <> f pts <+> "->" <+> pTerm f l t
pTerm f l (App t1 t2) = pTerm f l t1 <+> parensTerm f l t2
pTerm f l (Let pt t1 t2) = sep [ "let" <+> l pt <+> equals <+> pTerm f l t1 <+> "in"
                               , pTerm f l t2
                               ]
pTerm _ _ (Literal lit) = pLiteral lit
pTerm f l (Case t cases) = ("case" <+> pTerm f l t <+> "of") $+$
                           nest 2 (pCases (pTerm f l) cases)

parensTerm :: (fn -> Doc) -> (lt -> Doc) -> Term fn lt -> Doc
parensTerm f l t = case t of
    App (App (Con (ConN "(,)")) _) _ -> d
    App (App (App (Con (ConN "(,,)")) _) _) _ -> d
    Abs _ _ -> parens d
    Let _ _ _ -> parens d
    App _ _ -> parens d
    _ -> d
  where
    d = pTerm f l t

pTuple :: (a -> Doc) -> [a] -> Doc
pTuple f l = "(" <> hcat (intersperse ", " (map f l)) <> ")"

pPattern :: Pattern -> Doc
pPattern (VarPat v) = pVar v
pPattern (Pat (ConN "(,)") pts@[_, _]) = pTuple pPattern pts
pPattern (Pat (ConN "(,,)") pts@[_, _, _]) = pTuple pPattern pts
pPattern (Pat c pts) = parens (pCon c <+> hsep (map pPattern pts))
pPattern (IntPat i) = text i

pLiteral :: Literal -> Doc
pLiteral (IntLit i) = text i
pLiteral (RealLit r) = text r

pCases :: (a -> Doc) -> [(Pattern, a)] -> Doc
pCases tf (case' : cases) = (space <+> p case') $$
                            vcat (map (\case'' -> "|" <+> p case'') cases)
  where
    p (pt, t) = sep [pPattern pt <+> "->", nest 2 (tf t)]
pCases _ _ = "Parser.pCases: Received 0 cases"

pDecl :: (fn -> Doc) -> (lt -> Doc) -> Decl (Term fn lt) -> Doc
pDecl f l (ValDecl v t) = sep [pVar v <+> "=", nest 4 (pTerm f l t)] <> ";"
pDecl _ _ (TypeSig v ty) = pVar v <+> ":" <+> pQual ty <> ";"
pDecl _ _ (DataDecl c tyvs dbody)
    = "data" <+> pCon c <+> hsep (map pVar tyvs) <+> "where" $$
      nest 4 (pDataBody dbody) <> ";"
pDecl _ _ (ClassDecl pds tyc tyvs methods)
    = "class" <+> pConstraints pds <+> pCon tyc <+>
      hsep (map pVar tyvs) <+> "where" <+> "{" $$
      nest 4 (vcat (map (\(v, ty) -> pVar v <+> ":" <+> pQual ty) methods)) $$
      "}"
pDecl f l (ClassInst pds tyc tys methods)
    = "instance" <+> pConstraints pds <+> pCon tyc <+>
      hsep (map pType tys) <+> "where" <+> "{" $$
      nest 4 (vcat (map (\(v, t) -> sep [pVar v <+> "=", nest 4 (pTerm f l t)]) methods)) $$
      "}"

pDataBody :: DataBody -> Doc
pDataBody (opt : opts) = space <+> p opt $$ vcat (map (\opt' -> "|" <+> p opt') opts)
  where
    p (c, tss) = pCon c <+> hsep (map pType tss)
pDataBody _ = "Parser.pDataBody: Received 0 options"

pType :: Type -> Doc
pType (TyApp (TyApp (TyCon (ConN "(->)")) ts1) ts2) =
    pParensType ts1 <+> "->" <+> pType ts2
pType (TyApp (TyApp (TyCon (ConN "(,)")) ts1) ts2) = pTuple pType [ts1, ts2]
pType (TyApp (TyApp (TyApp (TyCon (ConN "(,,)")) ts1) ts2) ts3) =
    pTuple pType [ts1, ts2, ts3]
pType (TyVar tyv) = pVar tyv
pType (TyCon tyc) = pCon tyc
pType (TyApp ty1 ty2) = pType ty1 <+> pParensType ty2
pType (TyGen i) = text (show i)

pQual :: Qual -> Doc
pQual (pds :=> ty) = pConstraints pds <+> pType ty

pConstraints :: [Pred] -> Doc
pConstraints [] = empty
pConstraints pds = pPreds pds <+> "=>"

pPreds :: [Pred] -> Doc
pPreds = pTuple pPred

pPred :: Pred -> Doc
pPred (Pred tyc tys) = pCon tyc <+> hcat (map pParensType tys)

pParensType :: Type -> Doc
pParensType ty = case ty of
    TyApp (TyApp (TyCon (ConN "(,)")) _) _ -> d
    TyApp (TyApp (TyApp (TyCon (ConN "(,,)")) _) _) _ -> d
    TyApp _ _ -> parens d
    _ -> d
  where
    d = pType ty

-------------------------------------------------------------------------------

prettyScheme :: Scheme -> String
prettyScheme = render . pScheme

prettyType :: Type -> String
prettyType = render . pType

prettyAssump :: (a -> Doc) -> Assump a -> String
prettyAssump f = render . pAssump f

pAssump :: (a -> Doc) -> Assump a -> Doc
pAssump f m = vcat $ map (\(x, ty) -> text x <+> ":" <+> f ty) (Map.toList m)

pScheme :: Scheme -> Doc
pScheme (Forall _ ty) = pType ty

prettyKind :: Kind -> String
prettyKind = render . pKind

pKind :: Kind -> Doc
pKind Star = "*"
pKind (k1 :*> k2) = p k1 <+> "->" <+> pKind k2
  where
    p Star = "*"
    p k = parens (pKind k)
pKind (KVar v) = pVar v

-- prettySubst :: Subst -> String
-- prettySubst = render . vcat . map (\(tyv, ty) -> text tyv <+> "=>" <+> pType ty)

instance Show TypeError where
    show (TypeError s) = "TypeError: " ++ s
    show (UnboundVar v) = "Unboud variable \"" ++ unVar v ++ "\""
    show (UnboundConstructor c) = "Unbound constructor \"" ++ unCon c ++ "\""
    show (MismatchingKinds tyv k1 k2) =
        "Mismatching kinds for type variable " ++ tyv ++ ": \"" ++
        prettyKind k1 ++ "\" and \"" ++ prettyKind k2 ++ "\""
    show (UnboundTypeVar tyv) = "Unbound type variable \"" ++ unVar tyv ++ "\""
    show (UnboundTypeConstructor tyc) =
        "Unbound type constructor \"" ++ unCon tyc ++ "\""
    show (OccursCheck ty1 ty2) =
         "Occurs check fails when unifying \"" ++ prettyType ty1 ++ "\" with \"" ++
         prettyType ty2 ++ "\""
    show (KindOccursCheck k1 k2) =
         "Occurs check fails when unifying \"" ++ prettyKind k1 ++ "\" with \"" ++
         prettyKind k2 ++ "\""
    show (DifferentKinds ty1 k1 ty2 k2) =
         "Different kinds, \"" ++ prettyType ty1 ++ " : " ++ prettyKind k1 ++
         "\" and \"" ++ prettyType ty2 ++ " : " ++ prettyKind k2
    show (TooGeneralTypeSig ts ty) =
         "Type signature \"" ++ prettyScheme ts ++
         "\" is too general for inferred type \"" ++ prettyScheme ty ++ "\""
