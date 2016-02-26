{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}

module UntypedLambda where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Debug.Trace

-- |> Todo: move to Kinds
-- data Type where
--   TInt :: Type
--   TFunc :: Type -> Type -> Type

 -- data Value t where
 --   VVar   :: Name -> Value t
 --   VLambda :: Name -> Value t -> Value t
 --   VPrim:: Name -> Value t -> Value t
 --   VConst :: PrimVal -> Value t
 --   VFree :: Name -> Value t

data PrimVal = PrimInt Int
             deriving (Show, Eq, Ord)

type Name = String

data Value =
    VVar Name
  | VLambda Name Value
  | VPrim Name Value
  | VConst PrimVal
  | VApply Value  Value
  | VFree Name


deriving instance Show Value



data VContext = VContext (Map.Map Name Value) (Set.Set Name)
                deriving (Show)

lookupVContext :: VContext -> Name -> Maybe Value
lookupVContext (VContext contextMap _) name = Map.lookup name contextMap

bindInVContext :: VContext -> Name -> Value -> VContext
bindInVContext (VContext contextMap undefinedSet) name expr  = VContext newContextMap newUndefinedSet
  where
    newContextMap = Map.insert name expr contextMap
    newUndefinedSet = Set.delete name undefinedSet

defaultContext :: VContext
defaultContext = VContext (Map.fromList
                           [
                           ]) (Set.fromList [])


-- sorry :: forall a. a
-- sorry = error "didn't work yo"




eval :: Value -> VContext -> Value
eval input ctx = case input of
  VVar name         -> evalVVar name
  VLambda name expr ->  VLambda name expr
  VConst prim       ->  VConst prim
  VPrim name expr    ->  VPrim name expr
  VFree name        ->  VFree name
  VApply f x        -> evalVApply f x

  where
    evalVVar name = f $ lookupVContext ctx name
      where
        f (Just expr) = expr
        -- f Nothing     = error $ "Error! " ++ name ++ " not found!"
        f Nothing     = VFree name

    evalVApply leftExpr rightExpr = evalFirst (eval leftExpr ctx) (eval rightExpr ctx)
      where
        evalFirst (VLambda name expr) right = eval (expr) $ bindInVContext ctx name right
        evalFirst l r = VApply l r


-- testExpr = VApply (VLambda "x" (VVar "x")) (VVar "y")
testExpr = VApply (VLambda "x" (VVar "x")) (VConst $ PrimInt 2)

icomb = VLambda "x" (VVar "x")
kcomb = VLambda "x" (VLambda "y" (VVar "x"))
scomb = VLambda "x" (VLambda "y" (VLambda "z" (VApply (VApply (VVar "x") (VVar "z")) (VApply (VVar"y") (VVar "y")))))

omega = VApply om om
  where
    om = VLambda "x" (VApply (VVar "x") (VVar "x"))

ycomb = VLambda "y" (VApply y y)
  where
    y = VLambda "x" (VApply (VVar "y") (VApply (VVar "x") (VVar "x")))

-- kiomega = (VApply (VApply kcomb icomb) icomb)
-- komegai = (VApply (VApply kcomb icomb) icomb)
-- TODO: get KIOmega to work
testExpr1 = VApply icomb $ VFree "hello"

test :: Value -> Value
test input = eval input defaultContext
