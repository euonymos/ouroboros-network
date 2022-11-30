{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Test.Consensus.Mempool.StateMachine (tests) where

import           Control.Monad (void, when)
import           Control.Monad.Except (runExcept)
import           Data.Foldable hiding (toList)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff (Expr (..), genericToExpr)
import qualified Data.TreeDiff.OMap as TD
import           GHC.Generics

import           Control.Tracer (nullTracer)

import           Cardano.Crypto.Hash.Class
import           Cardano.Crypto.Hash.SHA256
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool hiding (getTxSize)
import           Ouroboros.Consensus.Mempool.Impl.Types (ForgeLedgerState (..),
                     tickLedgerState)
import           Ouroboros.Consensus.Mempool.TxSeq
import           Ouroboros.Consensus.Storage.ChainDB.API hiding
                     (getLedgerTablesAtFor)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike hiding (bracket)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine hiding ((:>))
import           Test.StateMachine.DotDrawing
import qualified Test.StateMachine.Types as QC
import           Test.StateMachine.Types (History (..), HistoryEvent (..))
import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.State
import           Ouroboros.Consensus.Mock.Ledger.UTxO (Expiry, Tx, TxIn, TxOut)

import           Test.Consensus.Mempool hiding (tests)

import           Test.Tasty
import           Test.Tasty.QuickCheck

{-------------------------------------------------------------------------------
  Datatypes
-------------------------------------------------------------------------------}

data Model blk r = Model {
    -- | The current tip on the mempool
    modelMempoolTip :: !( TickedLedgerState blk ValuesMK
                        , Bool -- Should it be reachable on the LedgerDB?
                        ) -- TODO discuss how it is used
    -- | The current tip on the ledgerdb
  , modelLedgerDBTip :: !(LedgerState blk ValuesMK)
    -- | The current list of transactions
  , modelTxs :: ![(GenTx blk, TicketNo)]
    -- | The maximum capacity of the mempool
  , modelCapacity :: !MempoolCapacityBytes
  , modelMempoolWasFilled :: !Bool
    -- | Seen txs by ticketNo
  , modelLastTicket :: !TicketNo
    -- | Seen states
    --
    -- As the point is what is used to identify a ledger state, we don't want it
    -- to be repeated (or we would need some more logic) -- TODO @js I think
    -- this can be done so that if the same state is put back, then we remove ti
    -- in Tips, but who knows.
  , modelSeenStates :: !(Set (Point blk))
  }

-- | The remaining capacity in a model
currentRemainingCap ::
     LedgerSupportsMempool blk
  => Model blk r
  -> MempoolCapacityBytes
currentRemainingCap Model{modelTxs, modelCapacity} =
    MempoolCapacityBytes
  $ getMempoolCapacityBytes modelCapacity
  - sum [ txInBlockSize tx
        | (tx, _) <- modelTxs
        ]

-- | Actions on the mempool
data Action blk r =
    -- | Add some transactions to the mempool
    NewTxs ![GenTx blk]
  | -- | Unconditionally sync with the ledger db
    SyncLedger
  | -- | Ask for the current snapshot
    GetSnapshot
  -- maybe add ? | GetSnapshotFor (Point blk)
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

-- | Events external to the mempool
data Event blk r = ChangeLedger
    !(LedgerState blk ValuesMK)
    !Bool -- ^ Is this a new fork? i.e. should the current mempool tip became
          -- unreachable? TODO use sth isomorphic with Bool
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

data Command blk r = Action !(Action blk r)
                   | Event  !(Event blk r)
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

instance CommandNames (Command blk) where
  cmdName (Action action) = cmdName action
  cmdName (Event event)   = cmdName event

  cmdNames :: forall r. Proxy (Command blk r) -> [String]
  cmdNames _ = cmdNames (Proxy @(Action blk r))
            ++ cmdNames (Proxy @(Event blk r))

data Response blk r =
    -- | Nothing to tell
    Void
  | -- | Return the contents of a snapshot
    GotSnapshot !(Snap blk)
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

{-------------------------------------------------------------------------------
  Model side
-------------------------------------------------------------------------------}

initModel ::
  ( LedgerSupportsMempool blk
  , ValidateEnvelope blk
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> Model blk r
initModel cfg initialState =
  Model {
    modelMempoolTip       = (ticked, True)
  , modelLedgerDBTip      = initialState
  , modelTxs              = []
  , modelCapacity         = MempoolCapacityBytes 400
  , modelLastTicket       = zeroTicketNo
  , modelSeenStates       = Set.singleton $ castPoint $ getTip initialState
  , modelMempoolWasFilled = False
  }
  where ticked = tick cfg initialState

generator ::
  ( Arbitrary (LedgerState blk ValuesMK)
  , GetTip (LedgerState blk ValuesMK)
  , StandardHash blk
  , UnTick blk
  )
  => (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
     -- ^ Transaction generator based on an state
  -> Model blk Symbolic
  -> Maybe (Gen (Command blk Symbolic))
generator gTxs Model{modelMempoolTip, modelSeenStates} =
   Just $ sized $ \n ->
    frequency
      [ (100, Action . NewTxs <$> (gTxs n . unTick . fst $ modelMempoolTip))
      , (10, pure $ Action SyncLedger)
      , (10, do
            b  <- arbitrary
            ls <- arbitrary `suchThat` (  (`notElem` modelSeenStates)
                                         . castPoint
                                         . getTip
                                       ) -- FIXME maybe not needed?
            pure $ Event $ ChangeLedger ls b)
      , (10, pure $ Action GetSnapshot)
      ]

data Snap blk = Snap
 { snapTxs         :: ![(GenTx blk, TicketNo)] }

mock ::
     Model blk Symbolic
  -> Command blk Symbolic
  -> GenSym (Response blk Symbolic)
mock model = \case
  Action (NewTxs _)      -> pure Void
  Action SyncLedger      -> pure Void
  Action GetSnapshot     -> pure $ GotSnapshot Snap {
         snapTxs = modelTxs
       }
  Event (ChangeLedger _ _) -> pure Void
  where
    Model {
        modelTxs
      } = model

transitions ::
     ( Eq (GenTx blk)
     , Eq (TickedLedgerState blk ValuesMK)
     , HasTxId (GenTx blk)
     , LedgerSupportsMempool blk
     , ToExpr (GenTx blk)
     , ToExpr (LedgerState blk ValuesMK)
     , ValidateEnvelope blk
     )
  => LedgerConfig blk
  -> Model blk r
  -> Command blk r
  -> Response blk r
  -> Model blk r
transitions cfg model cmd resp = case (cmd, resp) of
  (Action (NewTxs txs), Void) ->
    if reachable
    then
      let nextTicket = succ $ modelLastTicket model
          (res, tk, _, st) = foldTxs cfg nextTicket (currentRemainingCap model)
                              mtip
                            $ txs
          modelTxs'     = mtxs ++
                        zip [ txForgetValidated vtx
                            | MempoolTxAdded vtx <- fst res
                            ]
                            [nextTicket..]
      in
        model {
           modelMempoolTip = (st, reachable)
         , modelTxs        = modelTxs'
         , modelLastTicket = if tk == zeroTicketNo
                             then zeroTicketNo
                             else pred tk
         , modelCapacity   = MempoolCapacityBytes 400
         , modelMempoolWasFilled = modelMempoolWasFilled || (not . null . snd $ res)
         }
    else
      let model' = transitions cfg model (Action SyncLedger) Void
      in transitions cfg model' cmd Void

  (Event (ChangeLedger l' b'), Void) ->
        model { modelMempoolTip  = (mtip, reachable && not b')
              , modelLedgerDBTip = l'
              , modelSeenStates  = Set.insert (castPoint $ getTip l') seenStates
              }
  (Action GetSnapshot, GotSnapshot{}) -> model
  (Action SyncLedger, Void) ->
    if mtip == tick cfg dbtip
    then model
    else
      let modelTxs'       = fmap fst mtxs
          (res, _, _, st) = foldTxs cfg zeroTicketNo modelCapacity
                              (tick cfg dbtip)
                              modelTxs'
          filteredTxSeq   =
            [ (tx, tk) | (tx, tk) <- mtxs
                          , tx
                            `elem` [ txForgetValidated vtx
                                   | MempoolTxAdded vtx <- fst res
                                   ]
                     ]
      in
        model {
          modelMempoolTip = (st, True)
        , modelTxs = filteredTxSeq
        , modelCapacity = MempoolCapacityBytes 400
        }

  _ -> error $ "mismatched command "
             <> show cmd
             <> " and response "
             <> show resp

  where
    Model {
        modelMempoolTip  = (mtip, reachable)
      , modelLedgerDBTip = dbtip
      , modelTxs         = mtxs
      , modelCapacity
      , modelSeenStates  = seenStates
      , modelMempoolWasFilled
      } = model

{-------------------------------------------------------------------------------
  Ledger helper functions
-------------------------------------------------------------------------------}

-- | Apply a list of transactions short-circuiting if the mempool gets full.
-- Emulates almost exactly the behavior of 'implTryAddTxs'.
foldTxs ::
  forall blk.
  ( LedgerSupportsMempool blk
  , BasicEnvelopeValidation blk
  )
  => LedgerConfig blk
  -> TicketNo
  -> MempoolCapacityBytes
  -> TickedLedgerState blk ValuesMK
  -> [GenTx blk]
  -> ( ([MempoolAddTxResult blk], [GenTx blk])
     , TicketNo
     , MempoolCapacityBytes
     , TickedLedgerState blk ValuesMK
     )
foldTxs cfg nextTk remainingCap initialState  =
    go ( []
       , nextTk
       , getMempoolCapacityBytes remainingCap
       , initialState
       )
  where
    go (acc, tk, cap, st) [] = ((reverse acc, [])
                               , tk
                               , MempoolCapacityBytes cap
                               , st
                               )
    go (acc, tk, cap, st) txs@(tx:next) =
      if cap < txInBlockSize tx
      then ((reverse acc, txs), tk, MempoolCapacityBytes cap, st)
      else
        let slot = case getTipSlot st of
              Origin -> minimumPossibleSlotNo (Proxy @blk)
              At v   -> v + 1
        in
        case runExcept $ applyTx cfg DoNotIntervene slot tx st of
          Left e           ->
            go ( MempoolTxRejected tx e:acc
               , tk
               , cap
               , st
               )
               next
          Right (st', vtx) ->
            go ( MempoolTxAdded vtx:acc
               , succ tk
               , cap - txInBlockSize tx
               , forgetLedgerTablesDiffsTicked st'
               )
               next

tick ::
  ( TickedTableStuff (LedgerState blk)
  , ValidateEnvelope blk
  , LedgerSupportsMempool blk
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> TickedLedgerState blk ValuesMK
tick cfg st =
  zipOverLedgerTablesTicked
    (flip rawApplyDiffs)
    ticked
    (projectLedgerTables st)
  where
    ticked = snd
           . tickLedgerState cfg
           . ForgeInUnknownSlot
           . forgetLedgerTables
           $ st

{-------------------------------------------------------------------------------
  SUT side
-------------------------------------------------------------------------------}

-- | The System Under Test
data SUT m blk =
  SUT
  !(Mempool m blk)
    -- ^ A Mempool
  !(StrictTVar m (Tips blk))
    -- ^ A TVar with tips. Emulates a ledger db to the extent needed by the
    -- ledger interface.
  deriving Generic

deriving instance ( NoThunks (Mempool m blk)
                  , NoThunks (StrictTVar m (Tips blk))
                  ) =>  NoThunks (SUT m blk)

-- | The tip of the ledger db and the one from the mempool. The ledger interface
-- will serve the values of the latter if the mempool requests it and is
-- reachable.
data Tips blk = Tips {
    ldbTip     :: !(LedgerState blk ValuesMK)
  , mempoolTip :: !(LedgerState blk ValuesMK, Bool)
  } deriving (Generic)

-- | Create a ledger interface and provide the tvar to modify it when switching
-- ledgers.
newLedgerInterface ::
  ( MonadSTM m
  , NoThunks (Tips blk)
  , GetTip (LedgerState blk ValuesMK)
  , LedgerSupportsMempool blk
  )
  => LedgerState blk ValuesMK
  -> m (LedgerInterface m blk, StrictTVar m (Tips blk))
newLedgerInterface initialLedger = do
  t <- newTVarIO $ Tips initialLedger (initialLedger, True)
  pure $ (,t) $ LedgerInterface {
      getCurrentLedgerState = forgetLedgerTables . ldbTip <$> readTVar t
    , getLedgerTablesAtFor  = \pt txs -> do
        let keys = foldl' (zipLedgerTables (<>)) polyEmptyLedgerTables
                 $ map getTransactionKeySets txs
        Tips ti (ot, reachable) <- atomically $ readTVar t
        if pt == castPoint (getTip ti)
        then
          let tbs = zipLedgerTables f keys $ projectLedgerTables ti
          in  pure $ Right $ tbs
        else if pt == castPoint (getTip ot) && reachable
             then
               let tbs = zipLedgerTables f keys $ projectLedgerTables ot
               in  pure $ Right $ tbs
             else pure $ Left $ PointNotFound pt
    }
 where
   f :: Ord k => KeysMK k v -> ValuesMK k v -> ValuesMK k v
   f (ApplyKeysMK (DS.Keys s)) (ApplyValuesMK (DS.Values v)) =
      ApplyValuesMK (DS.Values (Map.restrictKeys v s))

-- | Make a SUT
mkSUT ::
  ( MonadSTM m
  , NoThunks (Tips blk)
  , GetTip (LedgerState blk ValuesMK)
  , IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> m (SUT m blk)
mkSUT cfg initialLedger = do
  (lif, t) <- newLedgerInterface initialLedger
  mempool <- openMempoolWithoutSyncThread
               lif
               cfg
               (MempoolCapacityBytesOverride $ MempoolCapacityBytes 400)
               nullTracer
               txInBlockSize
  pure (SUT mempool t)

semantics ::
     (MonadSTM m, LedgerSupportsMempool blk) =>
     Command blk Concrete
  -> StrictTVar m (SUT m blk)
  -> m (Response blk Concrete)
semantics cmd r = do
  SUT m t <- atomically $ readTVar r
  case cmd of
    Action (NewTxs txs) -> do
      atomically $ do
       Tips ti (_, reachable) <- readTVar t
       -- The mempool tip is not reachable so it will fail. Already change it
       when (not reachable) $ writeTVar t (Tips ti (ti, True))
      void $ tryAddTxs m DoNotIntervene txs
      pure Void

    Action SyncLedger   -> do
     void $ syncWithLedger m
     atomically $ do
       Tips ti _ <- readTVar t
       -- We just synced with the ledger
       writeTVar t (Tips ti (ti, True))
     pure Void

    Action GetSnapshot -> do
      snap <- atomically (getSnapshot m)
      pure
        . GotSnapshot
        $ Snap [ (txForgetValidated tx, tk)
               | (tx, tk) <- snapshotTxs snap
               ]

    Event (ChangeLedger l' becomesUnreachable) -> atomically $ do
       Tips _ (ot, reachable) <- readTVar t
       let ot' = if reachable
                 then (ot, not becomesUnreachable)
                 else (ot, reachable)
       writeTVar t (Tips l' ot')
       pure Void

{-------------------------------------------------------------------------------
  Conditions
-------------------------------------------------------------------------------}

noPreconditions :: Model blk Symbolic -> Command blk Symbolic -> Logic
noPreconditions = const $ const Top

postconditions ::
  ( LedgerSupportsMempool blk
  , Eq (GenTx blk)
  )
  => Model    blk Concrete
  -> Command  blk Concrete
  -> Response blk Concrete
  -> Logic
postconditions model (Action GetSnapshot) (GotSnapshot (Snap txs)) =
  modelTxs model .== txs
postconditions _ _ _ = Top

shrinker :: Model blk Symbolic
         -> Command blk Symbolic
         -> [Command blk Symbolic]
shrinker _ (Action (NewTxs txs)) =
  Action . NewTxs <$> shrinkList shrinkNothing txs
shrinker _ _ = []

{-------------------------------------------------------------------------------
  State Machine
-------------------------------------------------------------------------------}

sm ::
  ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , UnTick blk
  , IOLike m
  , NoThunks (Mempool m blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
  -> m (StateMachine (Model blk) (Command blk) m (Response blk))
sm cfg initialState gTxs = do
  ior <- newTVarIO =<< mkSUT cfg initialState
  pure $ StateMachine {
      QC.initModel     = initModel cfg initialState
    , QC.transition    = transitions cfg
    , QC.precondition  = noPreconditions
    , QC.postcondition = postconditions
    , QC.invariant     = Nothing
    , QC.generator     = generator gTxs
    , QC.shrinker      = shrinker
    , QC.semantics     = \c -> semantics c ior
    , QC.mock          = mock
    , QC.cleanup       = noCleanup
    }

smUnused ::
  ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , UnTick blk
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
  -> StateMachine (Model blk) (Command blk) IO (Response blk)
smUnused cfg initialState gTxs = StateMachine {
    QC.initModel     = initModel cfg initialState
  , QC.transition    = transitions cfg
  , QC.precondition  = noPreconditions
  , QC.postcondition = postconditions
  , QC.invariant     = Nothing
  , QC.generator     = generator gTxs
  , QC.shrinker      = shrinker
  , QC.semantics     = undefined
  , QC.mock          = mock
  , QC.cleanup       = noCleanup
  }

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_mempoolSequential ::
  forall blk . ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , ToExpr (TickedLedgerState blk ValuesMK)
  , UnTick blk
  , NoThunks (Mempool IO blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
     -- ^ Initial state
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
     -- ^ Transaction generator
  -> Property
prop_mempoolSequential cfg initialState gTxs = forAllCommands smUnused' Nothing $
  \cmds -> monadicIO
    (do
        (hist, model, res) <- runCommandsWithSetup sm' cmds
        prettyCommands smUnused' hist
          $ checkCommandNames cmds
          $ tabulate "Command sequence length"
              [QC.lengthCommands cmds `bucketiseBy` 10]
          $ tabulate "Maximum ticket number"
              [(\(TicketNo t) -> t) (modelLastTicket model) `bucketiseBy` 5]
          $ tabulate "Number of txs to add"
              [ show $ length txs
              | (_, Invocation (Action (NewTxs txs)) _) <- unHistory hist
              ]
          $ tabulate "Mempool was filled"
              [ show $ modelMempoolWasFilled model ]
          $ res === Ok
    )
  where
    smUnused' = smUnused cfg initialState gTxs
    sm'       = sm       cfg initialState gTxs

    bucketiseBy v n =
      let
        l = (v `div` n) * n
      in
        "[" <> show l <> "-" <> show (l + n) <> ")"


prop_mempoolParallel ::
  ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , ToExpr (TickedLedgerState blk ValuesMK)
  , UnTick blk
  , NoThunks (Mempool IO blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
  -> Property
prop_mempoolParallel cfg initialState gTxs = forAllParallelCommands smUnused' Nothing $
  \cmds -> monadicIO $ do
        res <- runParallelCommandsWithSetup sm' cmds
        prettyParallelCommandsWithOpts
          cmds
          (Just (GraphOptions "./mempoolParallel.png" Png))
          res
 where
   smUnused' = smUnused cfg initialState gTxs
   sm'       = sm       cfg initialState gTxs

tests :: TestTree
tests = testGroup "QSM"
        [ testProperty "sequential"
          $ prop_mempoolSequential testLedgerConfig testInitLedger
          $ \i -> fmap (fmap fst . fst) . genTxs i
        , testProperty "parallel"
          $ prop_mempoolParallel testLedgerConfig testInitLedger
          $ \i -> fmap (fmap fst . fst) . genTxs i
        ]

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Eq (Validated (GenTx blk)) => Eq (TxSeq (Validated (GenTx blk))) where
  s1 == s2 = toList s1 == toList s2

instance NoThunks (Mempool IO TestBlock) where
  showTypeOf _  = showTypeOf (Proxy @(Mempool IO TestBlock))
  wNoThunks _ _ = return Nothing

instance Show (TxId (GenTx blk)) => ToExpr (TxId (GenTx blk)) where
  toExpr x = App (show x) []

instance ( ToExpr (TxId (GenTx blk))
         , ToExpr (GenTx blk)
         , ToExpr (LedgerState blk ValuesMK)
         , ToExpr (TickedLedgerState blk ValuesMK)
         , LedgerSupportsMempool blk
         ) => ToExpr (Model blk r) where

  toExpr model = Rec "Model" $ TD.fromList
    [ ("mempoolTip", toExpr $ fst $ modelMempoolTip model)
    , ("reachable", toExpr $ snd $ modelMempoolTip model)
    , ("ledgerTip", toExpr $ modelLedgerDBTip model)
    , ("txs", toExpr $ modelTxs model)
    , ("capacity", toExpr $ getMempoolCapacityBytes $ modelCapacity model)
    , ("lastTicket", toExpr $ modelLastTicket model)]

instance ( ToExpr (TxId (GenTx blk))
         , ToExpr (GenTx blk)
         , ToExpr (TickedLedgerState blk ValuesMK)
         , ToExpr (LedgerState blk ValuesMK)
         , LedgerSupportsMempool blk) => Show (Model blk r) where
  show = show . toExpr

instance ToExpr (GenTx blk) => ToExpr (Action blk r) where
  toExpr (NewTxs txs) = App "NewTxs" $ map toExpr txs
  toExpr SyncLedger   = App "SyncLedger" []
  toExpr GetSnapshot  = App "GetSnapshot" []

instance ToExpr (LedgerState blk ValuesMK) => ToExpr (Event blk r) where
  toExpr (ChangeLedger ls b) =
      Rec "ChangeLedger" $ TD.fromList [ ("tip",  toExpr ls)
                                       , ("newFork", toExpr b) ]

instance ( ToExpr (LedgerState blk ValuesMK)
         , ToExpr (GenTx blk)) => ToExpr (Command blk r) where
  toExpr (Action act) = toExpr act
  toExpr (Event ev)   = toExpr ev

instance ToExpr (Command blk r) => Show (Command blk r) where
  show = show . toExpr

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk
         ) => ToExpr (TxTicket (Validated (GenTx blk))) where
  toExpr tkt =
    Rec "Ticket"
    $ TD.fromList [ ("number", toExpr $ txTicketNo tkt)
                  , ("tx", toExpr $ txForgetValidated $ txTicketTx tkt)
                  , ("size", toExpr $ txTicketTxSizeInBytes tkt)]

instance ( Show (ApplyTxErr blk)
         , ToExpr (GenTx blk)
         , LedgerSupportsMempool blk
         , ToExpr (Validated (GenTx blk))) => ToExpr (MempoolAddTxResult blk) where
  toExpr (MempoolTxAdded vtx)     = App "Added" [toExpr vtx]
  toExpr (MempoolTxRejected tx e) = App "Rejected" [toExpr tx, App (show e) [] ]

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk) => ToExpr (Response blk r) where

  toExpr Void = App "Void" []
  toExpr (GotSnapshot s) =
    Rec "Snap" $
      TD.fromList [ ("txs", toExpr $ snapTxs s) ]

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk) => Show (Response blk r) where
  show = show . toExpr

deriving newtype instance ToExpr (TicketNo)

deriving instance NoThunks (LedgerState blk ValuesMK) => NoThunks (Tips blk)

instance Arbitrary (LedgerState TestBlock ValuesMK) where
  arbitrary = sized $ \n -> do
    (txs, _) <- genValidTxs n testInitLedger
    case runExcept $ repeatedlyM (flip applyTxToLedger) txs testInitLedger of
      Left _   -> error "Must not happen"
      Right st -> pure st

instance ToExpr (TickedLedgerState TestBlock ValuesMK) where
   toExpr (TickedSimpleLedgerState st) = App "Ticked" [ toExpr st ]

instance ToExpr (LedgerState TestBlock ValuesMK) where
   toExpr (SimpleLedgerState st tbs) = Rec "LedgerState" $ TD.fromList
      [ ("state", toExpr $ mockTip st)
      , ("tables", toExpr tbs)]

deriving instance ToExpr (MockState TestBlock)
instance ToExpr (Point TestBlock) where
  toExpr p = App (show p) []

instance ToExpr (Hash SHA256 Tx) where
  toExpr s = App (show s) []

instance ToExpr Addr where
  toExpr a = App (show a) []

deriving instance ToExpr (GenTx TestBlock)
deriving instance ToExpr (Tx)
deriving instance ToExpr (Expiry)
deriving newtype instance ToExpr SlotNo

instance ToExpr (LedgerTables (LedgerState TestBlock) ValuesMK) where
  toExpr = genericToExpr

instance ToExpr (ValuesMK TxIn TxOut) where
  toExpr (ApplyValuesMK (DS.Values m)) = App "Values" [ toExpr m ]

class UnTick blk where
  unTick :: forall mk. TickedLedgerState blk mk ->  LedgerState blk mk

instance UnTick TestBlock where
  unTick = getTickedSimpleLedgerState
