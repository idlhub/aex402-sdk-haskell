{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AeX402.PDA
-- Description : PDA derivation utilities for AeX402 AMM
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
--
-- This module provides functions for deriving Program Derived Addresses (PDAs)
-- used by the AeX402 AMM program. PDAs are deterministic addresses derived
-- from seeds and a program ID using a cryptographic hash function.

module AeX402.PDA
    ( -- * PDA Derivation
      derivePda
    , findPda

      -- * Pool PDAs
    , derivePoolPda
    , deriveNPoolPda

      -- * Vault PDAs
    , deriveVault0Pda
    , deriveVault1Pda
    , deriveLpMintPda

      -- * Farming PDAs
    , deriveFarmPda
    , deriveUserFarmPda
    , deriveFarmVaultPda

      -- * Lottery PDAs
    , deriveLotteryPda
    , deriveLotteryEntryPda
    , deriveLotteryVaultPda

      -- * Registry PDAs
    , deriveRegistryPda

      -- * Utility Functions
    , seedsToBytes
    , pubkeyToSeed
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)

import AeX402.Types (Pubkey(..), mkPubkey, pubkeyBytes, zeroPubkey)
import AeX402.Constants (programIdBytes)

-- ============================================================================
-- PDA Derivation Core
-- ============================================================================

-- | The "ProgramDerivedAddress" marker appended to seeds before hashing
pdaMarker :: ByteString
pdaMarker = "ProgramDerivedAddress"

-- | Check if a point is on the ed25519 curve.
--
-- This is a simplified check - in production you'd use proper ed25519 validation.
-- A valid PDA must NOT be on the curve (hence the bump iteration).
isOnCurve :: ByteString -> Bool
isOnCurve bs
    | BS.length bs /= 32 = True  -- Invalid length, try next bump
    | otherwise =
        -- Simplified check: if high bit of last byte is set, likely on curve
        -- Real implementation would use actual ed25519 point decompression
        let lastByte = BS.index bs 31
        in lastByte >= 0x80

-- | Derive a PDA with a specific bump seed.
--
-- Returns Nothing if the derived address is on the ed25519 curve
-- (which would make it a valid keypair, not a PDA).
derivePda
    :: [ByteString]  -- ^ Seeds (without bump)
    -> Word8         -- ^ Bump seed
    -> ByteString    -- ^ Program ID (32 bytes)
    -> Maybe (Pubkey, Word8)
derivePda seeds bump programId =
    let allSeeds = seeds ++ [BS.singleton bump, programId, pdaMarker]
        combined = BS.concat allSeeds
        hashed = convert (hashWith SHA256 combined) :: ByteString
    in if not (isOnCurve hashed)
        then case mkPubkey hashed of
            Just pk -> Just (pk, bump)
            Nothing -> Nothing
        else Nothing

-- | Find a valid PDA by iterating through bump seeds from 255 down to 0.
--
-- Returns the first valid PDA found along with its bump seed.
findPda
    :: [ByteString]  -- ^ Seeds (without bump)
    -> ByteString    -- ^ Program ID (32 bytes)
    -> Maybe (Pubkey, Word8)
findPda seeds programId = go 255
  where
    go :: Word8 -> Maybe (Pubkey, Word8)
    go bump = case derivePda seeds bump programId of
        Just result -> Just result
        Nothing -> if bump > 0 then go (bump - 1) else Nothing

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Convert a list of seeds to a single ByteString
seedsToBytes :: [ByteString] -> ByteString
seedsToBytes = BS.concat

-- | Convert a Pubkey to a seed ByteString
pubkeyToSeed :: Pubkey -> ByteString
pubkeyToSeed = pubkeyBytes

-- ============================================================================
-- Pool PDAs
-- ============================================================================

-- | Derive the PDA for a 2-token pool.
--
-- Seeds: ["pool", mint0, mint1]
derivePoolPda
    :: Pubkey      -- ^ Token 0 mint
    -> Pubkey      -- ^ Token 1 mint
    -> Maybe (Pubkey, Word8)
derivePoolPda mint0 mint1 =
    findPda
        [ "pool"
        , pubkeyToSeed mint0
        , pubkeyToSeed mint1
        ]
        programIdBytes

-- | Derive the PDA for an N-token pool.
--
-- Seeds: ["npool", mint0, mint1, ...]
deriveNPoolPda
    :: [Pubkey]    -- ^ Token mints (2-8)
    -> Maybe (Pubkey, Word8)
deriveNPoolPda mints
    | length mints < 2 || length mints > 8 = Nothing
    | otherwise =
        findPda
            ("npool" : map pubkeyToSeed mints)
            programIdBytes

-- ============================================================================
-- Vault PDAs
-- ============================================================================

-- | Derive the PDA for token 0 vault.
--
-- Seeds: ["vault0", pool]
deriveVault0Pda
    :: Pubkey      -- ^ Pool address
    -> Maybe (Pubkey, Word8)
deriveVault0Pda pool =
    findPda
        [ "vault0"
        , pubkeyToSeed pool
        ]
        programIdBytes

-- | Derive the PDA for token 1 vault.
--
-- Seeds: ["vault1", pool]
deriveVault1Pda
    :: Pubkey      -- ^ Pool address
    -> Maybe (Pubkey, Word8)
deriveVault1Pda pool =
    findPda
        [ "vault1"
        , pubkeyToSeed pool
        ]
        programIdBytes

-- | Derive the PDA for LP mint.
--
-- Seeds: ["lp_mint", pool]
deriveLpMintPda
    :: Pubkey      -- ^ Pool address
    -> Maybe (Pubkey, Word8)
deriveLpMintPda pool =
    findPda
        [ "lp_mint"
        , pubkeyToSeed pool
        ]
        programIdBytes

-- ============================================================================
-- Farming PDAs
-- ============================================================================

-- | Derive the PDA for a farm.
--
-- Seeds: ["farm", pool]
deriveFarmPda
    :: Pubkey      -- ^ Pool address
    -> Maybe (Pubkey, Word8)
deriveFarmPda pool =
    findPda
        [ "farm"
        , pubkeyToSeed pool
        ]
        programIdBytes

-- | Derive the PDA for a user's farm position.
--
-- Seeds: ["user_farm", farm, user]
deriveUserFarmPda
    :: Pubkey      -- ^ Farm address
    -> Pubkey      -- ^ User address
    -> Maybe (Pubkey, Word8)
deriveUserFarmPda farm user =
    findPda
        [ "user_farm"
        , pubkeyToSeed farm
        , pubkeyToSeed user
        ]
        programIdBytes

-- | Derive the PDA for a farm's LP vault.
--
-- Seeds: ["farm_vault", farm]
deriveFarmVaultPda
    :: Pubkey      -- ^ Farm address
    -> Maybe (Pubkey, Word8)
deriveFarmVaultPda farm =
    findPda
        [ "farm_vault"
        , pubkeyToSeed farm
        ]
        programIdBytes

-- ============================================================================
-- Lottery PDAs
-- ============================================================================

-- | Derive the PDA for a lottery.
--
-- Seeds: ["lottery", pool]
deriveLotteryPda
    :: Pubkey      -- ^ Pool address
    -> Maybe (Pubkey, Word8)
deriveLotteryPda pool =
    findPda
        [ "lottery"
        , pubkeyToSeed pool
        ]
        programIdBytes

-- | Derive the PDA for a user's lottery entry.
--
-- Seeds: ["lottery_entry", lottery, user]
deriveLotteryEntryPda
    :: Pubkey      -- ^ Lottery address
    -> Pubkey      -- ^ User address
    -> Maybe (Pubkey, Word8)
deriveLotteryEntryPda lottery user =
    findPda
        [ "lottery_entry"
        , pubkeyToSeed lottery
        , pubkeyToSeed user
        ]
        programIdBytes

-- | Derive the PDA for a lottery's LP vault.
--
-- Seeds: ["lottery_vault", lottery]
deriveLotteryVaultPda
    :: Pubkey      -- ^ Lottery address
    -> Maybe (Pubkey, Word8)
deriveLotteryVaultPda lottery =
    findPda
        [ "lottery_vault"
        , pubkeyToSeed lottery
        ]
        programIdBytes

-- ============================================================================
-- Registry PDAs
-- ============================================================================

-- | Derive the PDA for the pool registry.
--
-- Seeds: ["registry"]
deriveRegistryPda :: Maybe (Pubkey, Word8)
deriveRegistryPda =
    findPda
        [ "registry" ]
        programIdBytes
