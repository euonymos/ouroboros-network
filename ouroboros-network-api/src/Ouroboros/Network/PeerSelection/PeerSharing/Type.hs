{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.PeerSelection.PeerSharing.Type
  ( PeerSharing (..)
  , combinePeerInformation
  , encodeRemoteAddress
  , decodeRemoteAddress
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),
                     withText)
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Network.Socket (PortNumber, SockAddr (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise.Type
                     (PeerAdvertise (..))

-- | Is a peer willing to participate in Peer Sharing? If yes are others allowed
-- to share this peer's address?
-- This information shall come from the Node's configuration file. Other peer's
-- willingness information is received via Handshake.
--
-- NOTE: This information is only useful if P2P flag is enabled.
--
data PeerSharing = NoPeerSharing -- ^ Peer does not participate in Peer Sharing
                                 -- at all
                 | PeerSharingPrivate -- ^ Peer participates in Peer Sharing but
                                      -- its address should be private
                 | PeerSharingPublic -- ^ Peer participates in Peer Sharing
  deriving  (Eq, Show, Read, Generic)

instance FromJSON PeerSharing where
  parseJSON = withText "PeerSharing" $
    return . read . Text.unpack

instance ToJSON PeerSharing where
  toJSON = String . Text.pack . show

-- Combine a 'PeerSharing' value and a 'PeerAdvertise' value into a
-- resulting 'PeerSharing' that can be used to decide if we should
-- share or not the given Peer. According to the following rules:
--
-- - If no PeerSharing value is known then there's nothing we can assess
-- - If a peer is not participating in Peer Sharing ignore all other information
-- - If a peer said it wasn't okay to share its address, respect that no matter what.
-- - If a peer was privately configured with DoNotAdvertisePeer respect that no matter
-- what.
--
combinePeerInformation :: Maybe PeerSharing -> PeerAdvertise -> Maybe PeerSharing
combinePeerInformation Nothing                   _                  = Nothing
combinePeerInformation (Just NoPeerSharing)      _                  = Just NoPeerSharing
combinePeerInformation (Just PeerSharingPrivate) _                  = Just PeerSharingPrivate
combinePeerInformation (Just PeerSharingPublic)  DoNotAdvertisePeer = Just PeerSharingPrivate
combinePeerInformation _                         _                  = Just PeerSharingPublic

instance Serialise PortNumber where
  encode = CBOR.encodeWord16 . fromIntegral
  decode = fromIntegral <$> CBOR.decodeWord16


-- | This encoder should be faithful to the PeerSharing
-- CDDL Specification.
--
-- See the network design document for more details
--
encodeRemoteAddress :: SockAddr -> CBOR.Encoding
encodeRemoteAddress (SockAddrInet pn w) = CBOR.encodeListLen 3
                           <> CBOR.encodeWord 0
                           <> encode w
                           <> encode pn
encodeRemoteAddress (SockAddrInet6 pn fi (w1, w2, w3, w4) si) = CBOR.encodeListLen 8
                                                <> CBOR.encodeWord 1
                                                <> encode w1
                                                <> encode w2
                                                <> encode w3
                                                <> encode w4
                                                <> encode fi
                                                <> encode si
                                                <> encode pn
encodeRemoteAddress (SockAddrUnix _) = error "Should never be encoding a SockAddrUnix!"

-- | This decoder should be faithful to the PeerSharing
-- CDDL Specification.
--
-- See the network design document for more details
--
decodeRemoteAddress :: CBOR.Decoder s SockAddr
decodeRemoteAddress = do
  _ <- CBOR.decodeListLen
  tok <- CBOR.decodeWord
  case tok of
    0 -> do
      w <- decode
      pn <- decode
      return (SockAddrInet pn w)
    1 -> do
      w1 <- decode
      w2 <- decode
      w3 <- decode
      w4 <- decode
      fi <- decode
      si <- decode
      pn <- decode
      return (SockAddrInet6 pn fi (w1, w2, w3, w4) si)
    _ -> fail ("Serialise.decode.SockAddr unexpected tok " ++ show tok)

