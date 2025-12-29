{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AeX402.Constants
-- Description : Constants for the AeX402 AMM program
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
--
-- This module contains all constants used by the AeX402 AMM program,
-- including the program ID, instruction discriminators, error codes,
-- and various protocol parameters.

module AeX402.Constants
    ( -- * Program ID
      programId
    , programIdBytes

      -- * Token Programs
    , tokenProgramId
    , token2022ProgramId

      -- * Pool Constants
    , minAmp
    , maxAmp
    , defaultFeeBps
    , adminFeePct
    , minSwap
    , minDeposit
    , newtonIterations
    , rampMinDuration
    , commitDelay
    , migrationFeeBps
    , maxTokens

      -- * Account Sizes
    , poolSize
    , nPoolSize

      -- * Analytics Constants
    , bloomSize
    , ohlcv24h
    , ohlcv7d
    , slotsPerHour
    , slotsPerDay

      -- * Instruction Discriminators
    , Discriminator(..)
    , discriminatorBytes

      -- * Account Discriminators
    , AccountDiscriminator(..)
    , accountDiscriminatorBytes

      -- * Error Codes
    , AeX402Error(..)
    , errorCode
    , errorMessage

      -- * TWAP Windows
    , TwapWindow(..)

      -- * Circuit Breaker Constants
    , cbPriceDevBps
    , cbVolumeMult
    , cbCooldownSlots
    , cbAutoResumeSlots

      -- * Rate Limiting Constants
    , rlSlotsPerEpoch

      -- * Governance Constants
    , govVoteSlots
    , govTimelockSlots
    , govQuorumBps
    , govThresholdBps

      -- * ML Brain Constants
    , mlGamma
    , mlAlpha
    , mlEpsilon
    , mlNumStates
    , mlNumActions
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word64)

-- ============================================================================
-- Program ID
-- ============================================================================

-- | The AeX402 AMM program ID on Solana (base58: 3AMM53MsJZy2Jvf7PeHHga3bsGjWV4TSaYz29WUtcdje)
programId :: String
programId = "3AMM53MsJZy2Jvf7PeHHga3bsGjWV4TSaYz29WUtcdje"

-- | The program ID as raw bytes (32 bytes)
programIdBytes :: ByteString
programIdBytes = BS.pack
    [ 0x1f, 0x7c, 0x18, 0x2d, 0x3c, 0x5b, 0x9a, 0x4d
    , 0x6e, 0x8f, 0xa0, 0xb1, 0xc2, 0xd3, 0xe4, 0xf5
    , 0x06, 0x17, 0x28, 0x39, 0x4a, 0x5b, 0x6c, 0x7d
    , 0x8e, 0x9f, 0xa0, 0xb1, 0xc2, 0xd3, 0xe4, 0xf5
    ]

-- | SPL Token program ID
tokenProgramId :: String
tokenProgramId = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

-- | Token-2022 program ID
token2022ProgramId :: String
token2022ProgramId = "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb"

-- ============================================================================
-- Pool Constants
-- ============================================================================

-- | Minimum amplification coefficient
minAmp :: Integer
minAmp = 1

-- | Maximum amplification coefficient
maxAmp :: Integer
maxAmp = 100000

-- | Default swap fee in basis points (0.30%)
defaultFeeBps :: Integer
defaultFeeBps = 30

-- | Admin fee percentage (50% of swap fee)
adminFeePct :: Integer
adminFeePct = 50

-- | Minimum swap amount (in lamports)
minSwap :: Integer
minSwap = 100000

-- | Minimum deposit amount (in lamports)
minDeposit :: Integer
minDeposit = 100000000

-- | Maximum Newton's method iterations
newtonIterations :: Int
newtonIterations = 255

-- | Minimum ramp duration in seconds (1 day)
rampMinDuration :: Integer
rampMinDuration = 86400

-- | Commit delay for timelocked operations (1 hour)
commitDelay :: Integer
commitDelay = 3600

-- | Migration swap fee in basis points (0.1337%)
migrationFeeBps :: Integer
migrationFeeBps = 1337

-- | Maximum tokens in N-token pool
maxTokens :: Int
maxTokens = 8

-- ============================================================================
-- Account Sizes
-- ============================================================================

-- | Pool account size in bytes
poolSize :: Int
poolSize = 1024

-- | N-Pool account size in bytes
nPoolSize :: Int
nPoolSize = 2048

-- ============================================================================
-- Analytics Constants
-- ============================================================================

-- | Bloom filter size in bytes
bloomSize :: Int
bloomSize = 128

-- | Number of hourly OHLCV candles stored
ohlcv24h :: Int
ohlcv24h = 24

-- | Number of daily OHLCV candles stored
ohlcv7d :: Int
ohlcv7d = 7

-- | Slots per hour (~400ms slots, 3600s/0.4s = 9000)
slotsPerHour :: Integer
slotsPerHour = 9000

-- | Slots per day
slotsPerDay :: Integer
slotsPerDay = 216000

-- ============================================================================
-- Instruction Discriminators
-- ============================================================================

-- | Instruction discriminators for all AeX402 handlers
data Discriminator
    -- Pool creation
    = DiscCreatePool
    | DiscCreateNPool
    | DiscInitT0Vault
    | DiscInitT1Vault
    | DiscInitLpMint
    -- Swaps
    | DiscSwap
    | DiscSwapT0T1
    | DiscSwapT1T0
    | DiscSwapN
    | DiscMigT0T1
    | DiscMigT1T0
    -- Liquidity
    | DiscAddLiq
    | DiscAddLiq1
    | DiscAddLiqN
    | DiscRemLiq
    | DiscRemLiqN
    -- Admin
    | DiscSetPause
    | DiscUpdateFee
    | DiscWithdrawFee
    | DiscCommitAmp
    | DiscRampAmp
    | DiscStopRamp
    | DiscInitAuth
    | DiscComplAuth
    | DiscCancelAuth
    -- Farming
    | DiscCreateFarm
    | DiscStakeLp
    | DiscUnstakeLp
    | DiscClaimFarm
    | DiscLockLp
    | DiscClaimUnlockedLp
    -- Lottery
    | DiscCreateLottery
    | DiscEnterLottery
    | DiscDrawLottery
    | DiscClaimLottery
    -- Registry
    | DiscInitRegistry
    | DiscRegisterPool
    | DiscUnregisterPool
    -- Oracle
    | DiscGetTwap
    -- Circuit Breaker
    | DiscSetCB
    | DiscResetCB
    -- Rate Limiting
    | DiscSetRL
    -- Oracle Config
    | DiscSetOracle
    -- Governance
    | DiscGovPropose
    | DiscGovVote
    | DiscGovExecute
    | DiscGovCancel
    -- Orderbook
    | DiscInitBook
    | DiscPlaceOrder
    | DiscCancelOrder
    | DiscFillOrder
    -- Concentrated Liquidity
    | DiscInitCLPool
    | DiscCLMint
    | DiscCLBurn
    | DiscCLCollect
    | DiscCLSwap
    -- Flash Loans
    | DiscFlashLoan
    | DiscFlashRepay
    -- Multi-hop
    | DiscMultiHop
    -- ML Brain
    | DiscInitML
    | DiscConfigML
    | DiscTrainML
    | DiscApplyML
    | DiscLogML
    -- Transfer Hook
    | DiscTHExec
    | DiscTHInit
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Get the 8-byte discriminator for an instruction (little-endian)
discriminatorBytes :: Discriminator -> ByteString
discriminatorBytes = \case
    -- Pool creation
    DiscCreatePool     -> BS.pack [0xf9, 0xe3, 0xa7, 0xc8, 0xd1, 0xe4, 0xb9, 0xf2]
    DiscCreateNPool    -> BS.pack [0x1b, 0x7c, 0xc5, 0xe5, 0xbc, 0x33, 0x9c, 0x27]
    DiscInitT0Vault    -> BS.pack [0x9f, 0x4a, 0x3e, 0x0f, 0x0d, 0x3b, 0x8c, 0x5e]
    DiscInitT1Vault    -> BS.pack [0x8a, 0x5e, 0x2d, 0x3b, 0x1c, 0x9f, 0x4e, 0x7a]
    DiscInitLpMint     -> BS.pack [0xf2, 0xe7, 0xb8, 0xc5, 0xa3, 0xe9, 0xd1, 0xf4]
    -- Swaps
    DiscSwap           -> BS.pack [0xc8, 0x87, 0x75, 0xe1, 0x91, 0x9e, 0xc6, 0x82]
    DiscSwapT0T1       -> BS.pack [0x2a, 0x4e, 0xf1, 0xe0, 0xb7, 0xf2, 0x2a, 0x64]
    DiscSwapT1T0       -> BS.pack [0xc8, 0xc4, 0x75, 0xac, 0x1b, 0x13, 0x0e, 0x3a]
    DiscSwapN          -> BS.pack [0xf8, 0xe5, 0xd9, 0xb2, 0xc7, 0xe3, 0xa8, 0xf1]
    DiscMigT0T1        -> BS.pack [0xd5, 0xe9, 0xb7, 0xc3, 0xa8, 0xf1, 0xe4, 0xd2]
    DiscMigT1T0        -> BS.pack [0xb8, 0x3d, 0x39, 0x26, 0x94, 0x77, 0x88, 0x18]
    -- Liquidity
    DiscAddLiq         -> BS.pack [0xa9, 0xe5, 0xd1, 0xb3, 0xf8, 0xc4, 0xe7, 0xa2]
    DiscAddLiq1        -> BS.pack [0xe6, 0x12, 0x2e, 0x3c, 0x4e, 0x8b, 0xc9, 0x51]
    DiscAddLiqN        -> BS.pack [0xf6, 0xe4, 0xe9, 0xb1, 0xa8, 0xc2, 0xf7, 0xe3]
    DiscRemLiq         -> BS.pack [0x02, 0xf9, 0xc5, 0x75, 0x2c, 0xbc, 0x54, 0x2e]
    DiscRemLiqN        -> BS.pack [0xb4, 0xb1, 0xe9, 0xd7, 0xc5, 0xa2, 0xe8, 0xb3]
    -- Admin
    DiscSetPause       -> BS.pack [0xc9, 0x6e, 0x0d, 0x7e, 0x2b, 0x76, 0x75, 0xe0]
    DiscUpdateFee      -> BS.pack [0x4a, 0x1f, 0x9d, 0x7c, 0x5b, 0x2e, 0x3a, 0x8f]
    DiscWithdrawFee    -> BS.pack [0xf8, 0xe7, 0xb1, 0xc8, 0xa2, 0xd3, 0xe5, 0xf9]
    DiscCommitAmp      -> BS.pack [0xc4, 0xe2, 0xb8, 0xa5, 0xf7, 0xe3, 0xd9, 0xc1]
    DiscRampAmp        -> BS.pack [0x6a, 0x8e, 0x2d, 0x7b, 0x3f, 0x5e, 0x1c, 0x9a]
    DiscStopRamp       -> BS.pack [0x53, 0x10, 0xa2, 0x15, 0xbb, 0x27, 0x94, 0x3c]
    DiscInitAuth       -> BS.pack [0xf4, 0xf8, 0xe1, 0xb3, 0xc9, 0xa7, 0xe2, 0xf5]
    DiscComplAuth      -> BS.pack [0xf5, 0xe1, 0xe9, 0xb7, 0xa4, 0xd2, 0xe8, 0xf6]
    DiscCancelAuth     -> BS.pack [0xf6, 0xe8, 0xb2, 0xd5, 0xc1, 0xa9, 0xe3, 0xf7]
    -- Farming
    DiscCreateFarm     -> BS.pack [0x5c, 0x5d, 0x1a, 0x2f, 0x8e, 0x0c, 0x7b, 0x6d]
    DiscStakeLp        -> BS.pack [0xf7, 0xe2, 0xb9, 0xb3, 0xa7, 0xe1, 0xd4, 0xf8]
    DiscUnstakeLp      -> BS.pack [0xbc, 0xf8, 0x34, 0x4e, 0x65, 0xbf, 0x66, 0x41]
    DiscClaimFarm      -> BS.pack [0x9b, 0xec, 0xd6, 0xe0, 0xb7, 0x62, 0x75, 0x07]
    DiscLockLp         -> BS.pack [0xec, 0x8c, 0x02, 0x5f, 0x01, 0x83, 0xfb, 0xfe]
    DiscClaimUnlockedLp -> BS.pack [0x1e, 0x8b, 0xe8, 0x5c, 0xf4, 0x93, 0x85, 0xca]
    -- Lottery
    DiscCreateLottery  -> BS.pack [0x3c, 0xdb, 0x93, 0x26, 0x94, 0x77, 0x88, 0x6c]
    DiscEnterLottery   -> BS.pack [0xfc, 0x48, 0xef, 0x4e, 0x3a, 0x38, 0x95, 0xe7]
    DiscDrawLottery    -> BS.pack [0x11, 0xbc, 0x7c, 0x4d, 0x5a, 0x22, 0x61, 0x13]
    DiscClaimLottery   -> BS.pack [0xf4, 0x3c, 0x9f, 0x15, 0x3f, 0x5e, 0x7b, 0x7e]
    -- Registry
    DiscInitRegistry   -> BS.pack [0x18, 0x07, 0x60, 0xf5, 0xd4, 0xc3, 0xb2, 0xa1]
    DiscRegisterPool   -> BS.pack [0x29, 0x18, 0x07, 0xf6, 0xe5, 0xd4, 0xc3, 0xb2]
    DiscUnregisterPool -> BS.pack [0x30, 0x29, 0x18, 0x07, 0xf6, 0xe5, 0xd4, 0xc3]
    -- Oracle
    DiscGetTwap        -> BS.pack [0x01, 0x74, 0x65, 0x67, 0x61, 0x70, 0x77, 0x74]
    -- Circuit Breaker
    DiscSetCB          -> BS.pack [0x01, 0xcb, 0x01, 0xcb, 0x01, 0xcb, 0x01, 0xcb]
    DiscResetCB        -> BS.pack [0x02, 0xcb, 0x02, 0xcb, 0x02, 0xcb, 0x02, 0xcb]
    -- Rate Limiting
    DiscSetRL          -> BS.pack [0x6c, 0x72, 0x01, 0x6c, 0x72, 0x01, 0x6c, 0x72]
    -- Oracle Config
    DiscSetOracle      -> BS.pack [0x04, 0x03, 0x02, 0x01, 0x6c, 0x63, 0x72, 0x6f]
    -- Governance
    DiscGovPropose     -> BS.pack [0x00, 0x70, 0x6f, 0x72, 0x70, 0x76, 0x6f, 0x67]
    DiscGovVote        -> BS.pack [0x00, 0x65, 0x74, 0x6f, 0x76, 0x76, 0x6f, 0x67]
    DiscGovExecute     -> BS.pack [0x63, 0x65, 0x78, 0x65, 0x76, 0x6f, 0x67, 0x00]
    DiscGovCancel      -> BS.pack [0x6c, 0x63, 0x6e, 0x63, 0x76, 0x6f, 0x67, 0x00]
    -- Orderbook
    DiscInitBook       -> BS.pack [0x6b, 0x6f, 0x6f, 0x62, 0x74, 0x69, 0x6e, 0x69]
    DiscPlaceOrder     -> BS.pack [0x64, 0x72, 0x6f, 0x65, 0x63, 0x61, 0x6c, 0x70]
    DiscCancelOrder    -> BS.pack [0x72, 0x6f, 0x6c, 0x65, 0x63, 0x6e, 0x61, 0x63]
    DiscFillOrder      -> BS.pack [0x65, 0x64, 0x72, 0x6f, 0x6c, 0x6c, 0x69, 0x66]
    -- Concentrated Liquidity
    DiscInitCLPool     -> BS.pack [0x01, 0x01, 0x6c, 0x6f, 0x6f, 0x70, 0x6c, 0x63]
    DiscCLMint         -> BS.pack [0x01, 0x01, 0x74, 0x6e, 0x69, 0x6d, 0x6c, 0x63]
    DiscCLBurn         -> BS.pack [0x01, 0x01, 0x6e, 0x72, 0x75, 0x62, 0x6c, 0x63]
    DiscCLCollect      -> BS.pack [0x63, 0x65, 0x6c, 0x6c, 0x6f, 0x63, 0x6c, 0x63]
    DiscCLSwap         -> BS.pack [0x01, 0x01, 0x70, 0x61, 0x77, 0x73, 0x6c, 0x63]
    -- Flash Loans
    DiscFlashLoan      -> BS.pack [0x61, 0x6f, 0x6c, 0x68, 0x73, 0x61, 0x6c, 0x66]
    DiscFlashRepay     -> BS.pack [0x70, 0x65, 0x72, 0x68, 0x73, 0x61, 0x6c, 0x66]
    -- Multi-hop
    DiscMultiHop       -> BS.pack [0x70, 0x6f, 0x68, 0x69, 0x74, 0x6c, 0x75, 0x6d]
    -- ML Brain
    DiscInitML         -> BS.pack [0x72, 0x62, 0x6c, 0x6d, 0x74, 0x69, 0x6e, 0x69]
    DiscConfigML       -> BS.pack [0x61, 0x72, 0x62, 0x6c, 0x6d, 0x67, 0x66, 0x63]
    DiscTrainML        -> BS.pack [0x00, 0x6c, 0x6d, 0x6e, 0x69, 0x61, 0x72, 0x74]
    DiscApplyML        -> BS.pack [0x00, 0x6c, 0x6d, 0x79, 0x6c, 0x70, 0x70, 0x61]
    DiscLogML          -> BS.pack [0x61, 0x74, 0x73, 0x6c, 0x6d, 0x67, 0x6f, 0x6c]
    -- Transfer Hook
    DiscTHExec         -> BS.pack [0x69, 0x25, 0x65, 0xc5, 0x4b, 0xfb, 0x66, 0x1a]
    DiscTHInit         -> BS.pack [0x2b, 0x22, 0x0d, 0x31, 0xa7, 0x58, 0xeb, 0xeb]

-- ============================================================================
-- Account Discriminators
-- ============================================================================

-- | Account type discriminators (8-byte ASCII strings)
data AccountDiscriminator
    = AccPool       -- ^ "POOLSWAP"
    | AccNPool      -- ^ "NPOOLSWA"
    | AccFarm       -- ^ "FARMSWAP"
    | AccUserFarm   -- ^ "UFARMSWA"
    | AccLottery    -- ^ "LOTTERY!"
    | AccLotEntry   -- ^ "LOTENTRY"
    | AccRegistry   -- ^ "REGISTRY"
    | AccMLBrain    -- ^ "MLBRAIN!"
    | AccCLPool     -- ^ "CLPOOL!!"
    | AccCLPos      -- ^ "CLPOSIT!"
    | AccOrderBook  -- ^ "ORDERBOK"
    | AccAIFee      -- ^ "AIFEE!!!"
    | AccTHMeta     -- ^ "THMETA!!"
    | AccGovProp    -- ^ "GOVPROP!"
    | AccGovVote    -- ^ "GOVVOTE!"
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Get the 8-byte discriminator for an account type
accountDiscriminatorBytes :: AccountDiscriminator -> ByteString
accountDiscriminatorBytes = \case
    AccPool      -> "POOLSWAP"
    AccNPool     -> "NPOOLSWA"
    AccFarm      -> "FARMSWAP"
    AccUserFarm  -> "UFARMSWA"
    AccLottery   -> "LOTTERY!"
    AccLotEntry  -> "LOTENTRY"
    AccRegistry  -> "REGISTRY"
    AccMLBrain   -> "MLBRAIN!"
    AccCLPool    -> "CLPOOL!!"
    AccCLPos     -> "CLPOSIT!"
    AccOrderBook -> "ORDERBOK"
    AccAIFee     -> "AIFEE!!!"
    AccTHMeta    -> "THMETA!!"
    AccGovProp   -> "GOVPROP!"
    AccGovVote   -> "GOVVOTE!"

-- ============================================================================
-- Error Codes
-- ============================================================================

-- | AeX402 program error codes
data AeX402Error
    = ErrPaused           -- ^ 6000: Pool is paused
    | ErrInvalidAmp       -- ^ 6001: Invalid amplification coefficient
    | ErrMathOverflow     -- ^ 6002: Math overflow
    | ErrZeroAmount       -- ^ 6003: Zero amount
    | ErrSlippage         -- ^ 6004: Slippage exceeded
    | ErrInvalidInvariant -- ^ 6005: Invalid invariant or PDA mismatch
    | ErrInsufficientLiq  -- ^ 6006: Insufficient liquidity
    | ErrVaultMismatch    -- ^ 6007: Vault mismatch
    | ErrExpired          -- ^ 6008: Expired or ended
    | ErrAlreadyInit      -- ^ 6009: Already initialized
    | ErrUnauthorized     -- ^ 6010: Unauthorized
    | ErrRampConstraint   -- ^ 6011: Ramp constraint violated
    | ErrLocked           -- ^ 6012: Tokens are locked
    | ErrFarmingError     -- ^ 6013: Farming error
    | ErrInvalidOwner     -- ^ 6014: Invalid account owner
    | ErrInvalidDisc      -- ^ 6015: Invalid account discriminator
    | ErrCpiFailed        -- ^ 6016: CPI call failed
    | ErrFull             -- ^ 6017: Orderbook/registry is full
    | ErrCircuitBreaker   -- ^ 6018: Circuit breaker triggered
    | ErrOracleError      -- ^ 6019: Oracle price validation failed
    | ErrRateLimit        -- ^ 6020: Rate limit exceeded
    | ErrGovernanceError  -- ^ 6021: Governance error
    | ErrOrderError       -- ^ 6022: Orderbook error
    | ErrTickError        -- ^ 6023: Invalid tick
    | ErrRangeError       -- ^ 6024: Invalid price range
    | ErrFlashError       -- ^ 6025: Flash loan error
    | ErrCooldown         -- ^ 6026: Cooldown period not elapsed
    | ErrMevProtection    -- ^ 6027: MEV protection triggered
    | ErrStaleData        -- ^ 6028: Stale data
    | ErrBiasError        -- ^ 6029: ML bias error
    | ErrDurationError    -- ^ 6030: Invalid duration
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Get the numeric error code
errorCode :: AeX402Error -> Word64
errorCode err = 6000 + fromIntegral (fromEnum err)

-- | Get a human-readable error message
errorMessage :: AeX402Error -> String
errorMessage = \case
    ErrPaused           -> "Pool is paused"
    ErrInvalidAmp       -> "Invalid amplification coefficient"
    ErrMathOverflow     -> "Math overflow"
    ErrZeroAmount       -> "Zero amount"
    ErrSlippage         -> "Slippage exceeded"
    ErrInvalidInvariant -> "Invalid invariant or PDA mismatch"
    ErrInsufficientLiq  -> "Insufficient liquidity"
    ErrVaultMismatch    -> "Vault mismatch"
    ErrExpired          -> "Expired or ended"
    ErrAlreadyInit      -> "Already initialized"
    ErrUnauthorized     -> "Unauthorized"
    ErrRampConstraint   -> "Ramp constraint violated"
    ErrLocked           -> "Tokens are locked"
    ErrFarmingError     -> "Farming error"
    ErrInvalidOwner     -> "Invalid account owner"
    ErrInvalidDisc      -> "Invalid account discriminator"
    ErrCpiFailed        -> "CPI call failed"
    ErrFull             -> "Orderbook/registry is full"
    ErrCircuitBreaker   -> "Circuit breaker triggered"
    ErrOracleError      -> "Oracle price validation failed"
    ErrRateLimit        -> "Rate limit exceeded"
    ErrGovernanceError  -> "Governance error"
    ErrOrderError       -> "Orderbook error"
    ErrTickError        -> "Invalid tick"
    ErrRangeError       -> "Invalid price range"
    ErrFlashError       -> "Flash loan error"
    ErrCooldown         -> "Cooldown period not elapsed"
    ErrMevProtection    -> "MEV protection triggered"
    ErrStaleData        -> "Stale data"
    ErrBiasError        -> "ML bias error"
    ErrDurationError    -> "Invalid duration"

-- ============================================================================
-- TWAP Windows
-- ============================================================================

-- | Time windows for TWAP oracle queries
data TwapWindow
    = TwapHour1   -- ^ 1 hour window
    | TwapHour4   -- ^ 4 hour window
    | TwapHour24  -- ^ 24 hour window
    | TwapDay7    -- ^ 7 day window
    deriving (Show, Eq, Ord, Enum, Bounded)

-- ============================================================================
-- Circuit Breaker Constants
-- ============================================================================

-- | Price deviation threshold in basis points (10%)
cbPriceDevBps :: Integer
cbPriceDevBps = 1000

-- | Volume multiplier trigger (10x average)
cbVolumeMult :: Integer
cbVolumeMult = 10

-- | Cooldown slots (~1 hour)
cbCooldownSlots :: Integer
cbCooldownSlots = 9000

-- | Auto-resume slots (~6 hours)
cbAutoResumeSlots :: Integer
cbAutoResumeSlots = 54000

-- ============================================================================
-- Rate Limiting Constants
-- ============================================================================

-- | Slots per epoch (~5 minutes)
rlSlotsPerEpoch :: Integer
rlSlotsPerEpoch = 750

-- ============================================================================
-- Governance Constants
-- ============================================================================

-- | Voting period in slots (~3 days)
govVoteSlots :: Integer
govVoteSlots = 518400

-- | Timelock delay in slots (~1 day)
govTimelockSlots :: Integer
govTimelockSlots = 172800

-- | Quorum in basis points (10%)
govQuorumBps :: Integer
govQuorumBps = 1000

-- | Pass threshold in basis points (50%)
govThresholdBps :: Integer
govThresholdBps = 5000

-- ============================================================================
-- ML Brain Constants
-- ============================================================================

-- | Q-learning discount factor
mlGamma :: Double
mlGamma = 0.9

-- | Q-learning rate
mlAlpha :: Double
mlAlpha = 0.1

-- | Exploration rate
mlEpsilon :: Double
mlEpsilon = 0.1

-- | Number of states (3^3 = 27)
mlNumStates :: Int
mlNumStates = 27

-- | Number of actions
mlNumActions :: Int
mlNumActions = 9
