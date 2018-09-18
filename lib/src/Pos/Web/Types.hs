-- | Types used by web server.

module Pos.Web.Types
       ( SscStage (..)
       , TlsParams (..)
       , TlsClientParams (..)
       , CConfirmedProposalState (..)
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveToJSON)

-- | Stages of SSC.
-- Also called GodTossing algorithm.
-- GodTossing is a coin tossing with guaranteed output delivery.
-- Nodes exchange commitments, openings, and shares, and in the end arrive at a shared seed.
-- See https://eprint.iacr.org/2016/889.pdf (“A Provably Secure Proof-of-Stake Blockchain Protocol”),
-- section 4 for more details.

data SscStage
    = CommitmentStage
    | OpeningStage
    | SharesStage
    | OrdinaryStage

deriveToJSON defaultOptions ''SscStage

-- | TLS Transport Layer Security file paths.
data TlsParams = TlsParams
    { tpCertPath   :: FilePath
    , tpCaPath     :: FilePath
    , tpKeyPath    :: FilePath
    , tpClientAuth :: Bool
    } deriving (Show)

-- | TLS Transport Layer Security client file paths
data TlsClientParams = TlsClientParams
    { tpClientCertPath :: !FilePath
    , tpClientKeyPath  :: !FilePath
    , tpClientCaPath   :: !FilePath
    } deriving (Show)


newtype CConfirmedProposalState = CConfirmedProposalState Text
    deriving (Show, Generic, Buildable)

deriveToJSON defaultOptions ''CConfirmedProposalState
