module Skiptracer.UnEval (
    unEval,
    unEvalUpto
) where

import           Skiptracer.Eval   (Ctx (..), State (..))
import qualified Skiptracer.Eval   as Eval
import           Skiptracer.Syntax (Alt (..), Exp (..))

-- | Generate an Exp from an Exp surrounded by Ctxs.
unEval :: Exp -> [Ctx] -> Exp
unEval = foldl unCtx

-- | Uneval upto a certain number of contexts, also returning whether the
-- expression was truncated early.
--
-- RefCtx and PatMatCtx are not counted since they don't touch the expression.
unEvalUpto :: Int -> Exp -> [Ctx] -> (Bool, Exp)
unEvalUpto _ e []                 = (False, e)
unEvalUpto 0 e _                  = (True, e)
unEvalUpto n e (RefCtx _ : cs)    = unEvalUpto n e cs
unEvalUpto n e (PatMatCtx _ : cs) = unEvalUpto n e cs
unEvalUpto n e (c : cs)           = unEvalUpto (n - 1) (unCtx e c) cs

-- | Unwrap an Exp in the context of a Ctx.
unCtx :: Exp -> Ctx -> Exp
unCtx e (ConMatCtx n es _ ps) = Con n (es ++ [e] ++ map snd ps)
unCtx e (AppCtx as)           = App e as
unCtx e (AppArgCtx fn as)     = App fn (e : as)
unCtx e (PopFstCtx op sn)     = App (Var op) [e, sn]
unCtx e (PopSndCtx op fs)     = App (Var op) [fs, e]
unCtx e (IteCtx te fe)        = Ite e te fe
unCtx e (CasMatCtx as)        = Cas e as
unCtx e (CasGrdCtx p b c as)  = Cas e (Alt p (Just e) b : as)
unCtx e (RefCtx _)            = e
unCtx e (PatMatCtx _)         = e
