{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

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
  | VLambda Name (Value )
  | VPrim Name (Value )
  | VConst PrimVal
  | VApply (Value ) (Value )
  | VFree Name


deriving instance Show (Value )



data VContext = VContext (Map.Map Name Value) (Set.Set Name)

lookupVContext :: VContext -> Name -> Maybe (Value )
lookupVContext (VContext contextMap _) name = Map.lookup name contextMap

bindInVContext :: VContext -> Name -> Value  -> VContext
bindInVContext (VContext contextMap undefinedSet) name expr  = VContext newContextMap newUndefinedSet
  where
    newContextMap = Map.insert name expr contextMap
    newUndefinedSet = Set.delete name undefinedSet

defaultContext :: VContext
defaultContext = VContext (Map.fromList
                           [ ("1", (VConst (PrimInt 1)))
                           , ("2", (VConst (PrimInt 2)))
                           , ("3", (VConst (PrimInt 3)))
                           ]) (Set.fromList [])


-- sorry :: forall a. a
-- sorry = error "didn't work yo"




eval :: Value -> VContext -> Value
eval input ctx = case input of
  VVar name -> evalVVar name
  VLambda name expr ->  VLambda name expr
  VConst prim ->  VConst prim
  VPrim str expr ->  VPrim str expr
  VFree name ->  VFree name
  VApply f x ->  evalVApply f x

  where
    evalVVar name = f $ lookupVContext ctx name
      where
        f (Just expr) = expr
        -- f Nothing     = error $ "Error! " ++ name ++ " not found!"
        f Nothing     = VFree name

    evalVApply leftExpr rightExpr = eval' (eval leftExpr ctx) (eval rightExpr ctx)
      where
        eval' (VLambda name expr) right = eval (traceShowId expr) $ bindInVContext ctx name right
        eval' left right = VApply left right



-- testExpr = VApply (VLambda "x" (VVar "x")) (VVar "y")
testExpr = VApply (VLambda "x" (VVar "x")) (VVar "2")

idcomb = VLambda "x" (VVar "x")
coolcomb = VLambda "x" (VLambda "y" (VVar"x"))

omega = VApply y y
  where
    y = VLambda "x" (VApply (VVar "x") (VVar "x"))

ycomb = VLambda "y" (VApply y y)
  where
    y = VLambda "x" (VApply (VVar "y") (VApply (VVar "x") (VVar "x")))

test :: Value -> Value
test input = eval input defaultContext
