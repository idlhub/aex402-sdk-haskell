-- |
-- Module      : AeX402
-- Description : Haskell SDK for the AeX402 AMM on Solana
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
-- Stability   : experimental
--
-- = AeX402 Haskell SDK
--
-- This is the main module for the AeX402 AMM SDK. It provides a pure Haskell
-- interface for interacting with the AeX402 hybrid AMM program on Solana.
--
-- == Features
--
-- * Dual pool types: Stable pools (high amp) and volatile pools (amp=1)
-- * N-token pools supporting 2-8 tokens with AeX402 StableSwap math
-- * Off-chain swap simulation using Newton's method
-- * Instruction data building for all 55+ handlers
-- * Account parsing for Pool, NPool, Farm, Lottery, etc.
-- * PDA derivation utilities
--
-- == Program ID
--
-- The AeX402 program is deployed on Solana devnet at:
--
-- > 3AMM53MsJZy2Jvf7PeHHga3bsGjWV4TSaYz29WUtcdje
--
-- == Quick Start
--
-- @
-- import AeX402
--
-- -- Simulate a swap
-- let result = simulateSwap
--       1000000000  -- bal0 (1 token with 9 decimals)
--       1000000000  -- bal1
--       10000000    -- amountIn (0.01 tokens)
--       100         -- amp (stable pool)
--       30          -- feeBps (0.30%)
--
-- case result of
--   Just amountOut -> print ("Output: " ++ show amountOut)
--   Nothing -> print "Swap simulation failed"
-- @
--
-- == Modules
--
-- * "AeX402.Constants" - Program ID, discriminators, error codes, constants
-- * "AeX402.Types" - Data types for pools, farms, lotteries, instruction args
-- * "AeX402.Accounts" - Binary account data parsing
-- * "AeX402.Instructions" - Instruction data builders
-- * "AeX402.Math" - StableSwap math using Newton's method
-- * "AeX402.PDA" - Program Derived Address utilities

module AeX402
    ( -- * Re-exports from Constants
      module AeX402.Constants

      -- * Re-exports from Types
    , module AeX402.Types

      -- * Re-exports from Accounts
    , module AeX402.Accounts

      -- * Re-exports from Instructions
    , module AeX402.Instructions

      -- * Re-exports from Math
    , module AeX402.Math

      -- * Re-exports from PDA
    , module AeX402.PDA
    ) where

import AeX402.Constants
import AeX402.Types
import AeX402.Accounts
import AeX402.Instructions
import AeX402.Math
import AeX402.PDA
