# Abstract Protocol

## Tutorials

### Overview

- [Ouroboros.Consensus.Tutorial.Simple](../src-docs/Ouroboros/Consensus/Tutorial/Simple.lhs):
  Simple round-robin instantiation of the abstract Ouroboros consensus protocol.
- [Ouroboros.Consensus.Tutorial.WithEpoch](../src-docs/Ouroboros/Consensus/Tutorial/WithEpoch.lhs):
  Example in which the leader schedule depends on data from the chain.

### Generating documents

From the `ouroboros-consensus` directory, run for your choice of `<output
file>`:

    pandoc -s -f markdown+lhs src-docs/Ouroboros/Consensus/Tutorial/Simple.lhs -o <output file>

## Overview

The following diagram depicts the relation between various constructs in the
abstract specification of the protocol and the consensus-level view of th
ledger.
- Red boxes indicate concepts, or informal kinds (e.g., `ShelleyLedgerState`
  would have the informal kind `Ledger`).
- Blue boxes indicate data families.
- Green boxes indicate type families.
- Type or data families map the block attached to the incoming arrow to the
  block attached to the outgoing arrow. When there is no outgoing arrow, the
  family maps to any type (e.g., `BlockConfig blk`).

```mermaid
flowchart LR;

  classDef df fill:#4285F4, border:10px;
  classDef tf fill:#34A853;
  classDef kind fill:#EA4335;

  Block(Block)-->BlockProtocol[BlockProtocol blk]; class Block kind;
  click Block "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  Block-->BlockConfig[BlockConfig blk]; class BlockConfig df
  BlockProtocol[BlockProtocol blk]-.->Protocol(Protocol); class BlockProtocol tf;
  click BlockProtocol "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  click BlockProtocol "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  click BlockConfig "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  Block-->StorageConfig; class StorageConfig df
  click StorageConfig "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  Block-->CodecConfig[CodecConfig blk]; class CodecConfig df
  click CodecConfig "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  Block-->Header[Header blk]; class Header df
  click Header "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Block/Abstract.hs"
  Ledger(Ledger); class Ledger kind
  click Ledger "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Ledger/Abstract.hs"
  Block-->LedgerState[LedgerState blk]; class LedgerState df;
  click LedgerState "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Ledger/Basics.hs"

  Block-->GenTx[GenTx blk]; class GenTx df
  click GenTx "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Ledger/SupportsMemPool.hs"
  Block-->ApplyTxErr[ApplyTxErr blk]; class ApplyTxErr df
  click ApplyTxErr "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Ledger/SupportsMemPool.hs"
  LedgerState-.->Ledger
  Ledger-->AuxLedgerEvent[AuxLedgerEvent l]; class AuxLedgerEvent tf
  click AuxLedgerEvent "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Ledger/Basics.hs"
  Ledger-->LedgerErr[LedgerErr l]; class LedgerErr tf
  click LedgerErr "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Ledger/Basics.hs"

  Protocol(Protocol); class Protocol kind
  Protocol-->ConsensusConfig[ConsensusConfig p]; class ConsensusConfig df
  click ConsensusConfig "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
  subgraph ConsensusProtocol[class ConsensusProtocol]
    ChainDepState[ChainDepState p]; class ChainDepState tf
    click ChainDepState "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
    IsLeader[IsLeader p]; class IsLeader tf
    click IsLeader "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
    CanBeLeader[CanBeLeader p]; class CanBeLeader tf
    click CanBeLeader "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
    SelectView[SelectView p]; class SelectView tf
    click SelectView "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
    LedgerView[LedgerView p]; class LedgerView tf
    click LedgerView "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
    ValidationErr[ValidationErr p]; class ValidationErr tf
    click ValidationErr "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
    ValidateView[ValidateView p]; class ValidateView tf
    click ValidateView "https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Protocol/Abstract.hs"
  end
  Protocol-->ChainDepState
  Protocol-->IsLeader[IsLeader p]
  Protocol-->CanBeLeader[CanBeLeader p]
  Protocol-->SelectView[SelectView p]
  Protocol-->LedgerView[LedgerView p]
  Protocol-->ValidationErr[ValidationErr p]
  Protocol-->ValidateView[ValidateView p]

```
