module Trace where

import Lang

aValidate :: ArExpr -> ArExpr
aValidate (Neg ArNoOp)              = ArNoOp
aValidate (ABinary _ _      ArNoOp) = ArNoOp
aValidate (ABinary _ ArNoOp _     ) = ArNoOp
aValidate a                         = a

bValidate :: BoolExpr -> BoolExpr
bValidate (Not BNoOp)               = BNoOp
bValidate (BBinary _ BNoOp _      ) = BNoOp
bValidate (BBinary _ _     BNoOp  ) = BNoOp
bValidate (RBinary _ ArNoOp _     ) = BNoOp
bValidate (RBinary _ _      ArNoOp) = BNoOp
bValidate b                         = b

sValidate :: Stmt -> Stmt
sValidate (BL BNoOp)      = NoOp
sValidate (AR ArNoOp)     = NoOp
sValidate (Let _ NoOp)    = NoOp
sValidate (While BNoOp _) = NoOp
sValidate (Seq xs)        = Seq $ fmap sValidate xs
sValidate s               = s

holeAr :: Var -> ArExpr -> ArExpr
holeAr v e@(V var)
  | v == var                 = e
  | otherwise                = ArNoOp
holeAr v (Neg arExpr)        = aValidate . Neg $ holeAr v arExpr
holeAr v (ABinary o a1 a2)   = aValidate $ ABinary o (holeAr v a1) (holeAr v a2)
holeAr _ e                   = e

holeBl :: Var -> BoolExpr -> BoolExpr
holeBl v (RBinary op a1 a2) = bValidate $ RBinary op (holeAr v a1) (holeAr v a2)
holeBl v (Not b)            = bValidate . Not $ holeBl v b
holeBl _ e                  = e

holify :: Var -> Stmt -> Stmt
holify v (AR a')     = sValidate . AR $ holeAr v a'
holify v (BL b')     = sValidate . BL $ holeBl v b'
holify v (Let var stmt)
  | v == var  = Let var . sValidate $ holify v stmt
  | otherwise = NoOp 
holify v (If c t e)  = sValidate $ If
                         (bValidate $ holeBl v c)
                         (sValidate $ holify v t)
                         (sValidate $ holify v e)
holify v (While c e) = sValidate $ While
                         (bValidate $ holeBl v c)
                         (sValidate $ holify v e)
holify v (Seq xs)    = Seq $ fmap (holify v) xs
holify _ s           = s

