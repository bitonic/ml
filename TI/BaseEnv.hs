module TI.BaseEnv
       ( intCon
       , realCon
       , baseTypes
       , baseKinds
       ) where

import qualified Data.Map as Map

import TI.TypesTypes
import Syntax

intCon, realCon, boolCon :: Type
intCon = TyCon (con "Int")
realCon = TyCon (con "Real")
boolCon = TyCon (con "Bool")

baseKinds :: Assump Kind
baseKinds = Map.fromList
    [ ("(->)", Star :*> Star :*> Star)
    , ("Bool", Star)
    , ("(,)", Star :*> Star :*> Star)
    , ("(,,)", Star :*> Star :*> Star :*> Star)
    , ("Int", Star)
    , ("Real", Star)
    ]

baseTypes :: Assump Scheme
baseTypes = Map.fromList
    [ ("(,)",
       ( Forall [Star, Star]
         (TyGen 0 --> TyGen 1 -->
          TyApp (TyApp (TyCon (con "(,)")) (TyGen 0)) (TyGen 1))
       ))
    , ("(,,)",
       ( Forall [Star, Star, Star]
         (TyGen 0 --> TyGen 1 --> TyGen 2 -->
          TyApp (TyApp (TyApp (TyCon (con "(,,)")) (TyGen 0)) (TyGen 1)) (TyGen 2))
       ))
    , ("plus", Forall [] (intCon --> intCon --> intCon))
    , ("negate", Forall [] (intCon --> intCon))
    , ("isZero", Forall [] (intCon --> boolCon))
    , ("True", Forall [] boolCon)
    , ("False", Forall [] boolCon)
    ]
