{-# OPTIONS -W -Wwarn #-}
{-# LANGUAGE LambdaCase #-}
module PlutusIR.Transform.NewLetFloat where

import           Control.Lens            hiding (Strict)
import           Control.Monad.Reader
import           Data.Coerce
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import           Data.Semigroup.Foldable
import           Data.Set                (Set, (\\))
import           Data.Traversable
import           GHC.Exts
import qualified PlutusCore              as PLC
import qualified PlutusCore.Name         as PLC
import           PlutusIR
import           PlutusIR.Core.Plated

-- don't break down rec groups
-- | Selects a binding to be used a representative binding in MARKING the group of bindings.
representativeBinding :: NE.NonEmpty (Binding tyname name uni fun a) -> Binding tyname name uni fun a
representativeBinding = NE.head

-- this is used for two purposes
-- 1) to mark the max-position of a var in scope
-- 2) add lambdas
type Env = M.Map PLC.Unique Pos

-- a position is not just the lambda-depth level because we act globally, it is also the unique of lambda/Lambda
type Pos = (Depth, PLC.Unique)
type Depth = Int

type Marks = M.Map PLC.Unique Pos

type LetHoled tyname name uni fun a = (a, Recursivity, NE.NonEmpty (Binding tyname name uni fun a))


mark :: Term TyName Name uni fun a -> Marks
mark t = runReader (go t) mempty
    where
      go :: Term TyName Name uni fun a -> Reader Env Marks
      go = \case
          LamAbs _ n _ tBody  -> withLam (n^.PLC.theUnique) $ go tBody
          TyAbs _ n _ tBody   -> withLam (n^.PLC.theUnique) $ go tBody
          -- breaks down the NonRec true-groups
          Let a NonRec bs tIn | length bs > 1 ->
                                mconcat . NE.toList <$> for bs (\ b -> go (Let a NonRec (pure b) tIn))
          l@(Let _ r bs tIn) ->
              if unmovable l
              then do
                  -- since it is unmovable, the floatpos is the max pos, which is the enclosing l/Lam
                  pos <- maxPos <$> ask
                  marked1 <- (if isRec r then withBs bs pos else id) (mconcat <$> traverse go (bs^..traversed.bindingSubterms))
                  marked2 <- withBs bs pos $ go tIn
                  -- don't add any marks
                  pure $ marked1 <> marked2
              else do
                  env <- ask
                  let freeVars = (if isRec r then  (\\ fromList (bs^..traversed.bindingIds)) else id) $ calcFreeVars l env
                      pos =  maxPos $ M.restrictKeys env freeVars
                  marks1 <- (if isRec r then withBs bs pos else id) (mconcat <$> traverse go (bs^..traversed.bindingSubterms))
                  marks2 <- withBs bs pos $ go tIn
                  -- add here a new mark
                  pure $ M.singleton (head $ representativeBinding bs^..bindingIds) pos <> marks1 <> marks2
          t' -> mconcat <$> traverse go (t'^..termSubterms)

calcFreeVars :: (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique) => Term tyname name uni fun ann
             -> M.Map PLC.Unique a -> Set PLC.Unique
calcFreeVars t env = fromList (t^..termUniquesDeep) \\ M.keysSet env


removeLets :: Marks -> Term TyName Name uni fun a -> (M.Map Pos (NE.NonEmpty (LetHoled TyName Name uni fun a)) -- letterms
                            , Term TyName Name uni fun a)
removeLets ms = go
    where
      go = \case
          -- break down let nonrec true group
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)
          Let a r@Rec bs tIn ->
              let (m', tIn') = go tIn
              in case M.lookup (head $ representativeBinding bs^..bindingIds) ms of
                  Nothing  -> (m', Let a r bs tIn)
                  Just pos -> (M.insertWith (<>) pos (pure (a,r,bs)) m', tIn')
          t' -> t' & termSubterms go


floatBackLets :: -- | remove result
                (M.Map Pos (NE.NonEmpty (LetHoled TyName Name uni fun a)) -- letterms
                , Term TyName Name uni fun a)
              -> Term TyName Name uni fun a
floatBackLets (letholesTable,t) =
    -- toplevel lets
    maybe id mergeLetsIn (M.lookup (0,topUnique) letholesTable) $ runReader (go t) 0
    where go = \case
              LamAbs a n ty tBody -> goLam (LamAbs a n ty) (n^.PLC.theUnique) tBody
              TyAbs a n k tBody   -> goLam (TyAbs a n k) (n^.PLC.theUnique) tBody
              t'                  -> t' & termSubterms go

          goLam k u tBody = local (+1) $ do
              depth <- ask
              tBody' <- go tBody
              pure $ k $ case M.lookup (depth, u) letholesTable of
                           Just letholes -> mergeLetsIn letholes tBody'
                           Nothing       ->  tBody'

mergeLetsIn :: NE.NonEmpty (LetHoled TyName Name uni fun a) -> Term TyName Name uni fun a -> Term TyName Name uni fun a
mergeLetsIn ls = Let (NE.head ls^._1)
                 Rec -- needs to be rec because we don't do dep resolution
                 (foldMap1 (^._3) ls)

floatTerm :: Term TyName Name uni fun a -> Term TyName Name uni fun a
floatTerm t = floatBackLets $ removeLets (mark t) t

-- NOTES:
-- 1) no adjacent let-nonrec merging, it is done by the new letmerge pass
--
-- 2) compared to "Let-floating: moving bindings to give faster programs", the algorithm
-- does not float right outside the free-lamdba, but right inside the dependent lambda


-- MISSING, TODO:
-- let-group splitting and correct ordering based on dep.resolution; right now is 1 big letrec at every floating position
-- dep.resolution does not necessarily need depgraph,scc,topsort, it can be done by an extra int dep inside the Marks
-- change unmovable to allow moving of strict-pure
-- Skip marking big Lambdas which as outlined in the paper

-- OPTIMIZE:
-- recursive descend for removelets, floatbacklets does not shrink search space; fix: keep a state
-- use some better/safer free-vars calculation?
-- keep an extra depth reader in mark pass, instead of relying on maxPos for l/Lambdas

-- HELPERS

-- dummy unique to signify toplevel
topUnique :: PLC.Unique
topUnique = coerce (-1 :: Int)

maxDepth env = fst $ maxPos env
maxPos env = foldl max (0, topUnique) $ M.elems env

withLam lamInt = local $ \ env ->
                            let lamPos = (maxDepth env + 1, lamInt)
                            in M.insert lamInt lamPos env

withBs bs pos = local $ \ env -> M.fromList [(bid,pos) | bid <- bs^..traversed.bindingIds]
                                <> env

unmovable (Let _ NonRec (b  NE.:|[]) _) = isStrictBinding b
unmovable (Let _ Rec bs _)              = any isStrictBinding bs
unmovable _                             = error "unmovable not total"

-- FIXME: boolean blindness
isStrictBinding (TermBind _ Strict _  _) = True
isStrictBinding _                        = False

-- FIXME: boolean blindness
isRec Rec = True
isRec _   = False


