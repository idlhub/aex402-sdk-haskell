{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AeX402.Instructions
-- Description : Instruction data builders for AeX402 AMM
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
--
-- This module provides functions for building instruction data
-- for the AeX402 AMM program. The instruction data is serialized
-- as little-endian bytes ready to be included in Solana transactions.

module AeX402.Instructions
    ( -- * Pool Creation
      createPoolData
    , createNPoolData
    , initT0VaultData
    , initT1VaultData
    , initLpMintData

      -- * Swaps
    , swapData
    , swapT0T1Data
    , swapT1T0Data
    , swapNData

      -- * Liquidity
    , addLiquidityData
    , addLiquidity1Data
    , removeLiquidityData

      -- * Admin
    , setPauseData
    , updateFeeData
    , withdrawFeeData
    , commitAmpData
    , rampAmpData
    , stopRampData
    , initAuthTransferData
    , completeAuthTransferData
    , cancelAuthTransferData

      -- * Farming
    , createFarmData
    , stakeLpData
    , unstakeLpData
    , claimFarmData
    , lockLpData
    , claimUnlockedLpData

      -- * Lottery
    , createLotteryData
    , enterLotteryData
    , drawLotteryData
    , claimLotteryData

      -- * Oracle
    , getTwapData

      -- * Utilities
    , writeU8
    , writeU64LE
    , writeI64LE
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8, Word64)
import Data.Int (Int64)

import AeX402.Constants
import AeX402.Types

-- ============================================================================
-- Binary Writing Helpers
-- ============================================================================

-- | Convert a ByteString builder to strict ByteString
toStrict :: BSB.Builder -> ByteString
toStrict = BSL.toStrict . BSB.toLazyByteString

-- | Write a u8
writeU8 :: Word8 -> BSB.Builder
writeU8 = BSB.word8

-- | Write a u64 in little-endian
writeU64LE :: Integer -> BSB.Builder
writeU64LE n = mconcat [BSB.word8 (fromIntegral $ (n `div` (256^i)) `mod` 256) | i <- [0..7]]

-- | Write an i64 in little-endian (same as u64)
writeI64LE :: Integer -> BSB.Builder
writeI64LE = writeU64LE

-- | Build instruction data with discriminator and payload
buildInstruction :: Discriminator -> BSB.Builder -> ByteString
buildInstruction disc payload =
    toStrict $ BSB.byteString (discriminatorBytes disc) <> payload

-- ============================================================================
-- Pool Creation Instructions
-- ============================================================================

-- | Build instruction data for createpool
createPoolData :: CreatePoolArgs -> ByteString
createPoolData CreatePoolArgs{..} =
    buildInstruction DiscCreatePool $
        writeU64LE cpaAmp <>
        writeU8 cpaBump

-- | Build instruction data for createpn (N-pool)
createNPoolData :: CreateNPoolArgs -> ByteString
createNPoolData CreateNPoolArgs{..} =
    buildInstruction DiscCreateNPool $
        writeU64LE cnpaAmp <>
        writeU8 cnpaNTokens <>
        writeU8 cnpaBump

-- | Build instruction data for initt0v
initT0VaultData :: ByteString
initT0VaultData = discriminatorBytes DiscInitT0Vault

-- | Build instruction data for initt1v
initT1VaultData :: ByteString
initT1VaultData = discriminatorBytes DiscInitT1Vault

-- | Build instruction data for initlpm
initLpMintData :: ByteString
initLpMintData = discriminatorBytes DiscInitLpMint

-- ============================================================================
-- Swap Instructions
-- ============================================================================

-- | Build instruction data for generic swap
swapData :: SwapArgs -> ByteString
swapData SwapArgs{..} =
    buildInstruction DiscSwap $
        writeU8 saFrom <>
        writeU8 saTo <>
        writeU64LE saAmountIn <>
        writeU64LE saMinOut <>
        writeI64LE saDeadline

-- | Build instruction data for swapt0t1
swapT0T1Data :: SwapSimpleArgs -> ByteString
swapT0T1Data SwapSimpleArgs{..} =
    buildInstruction DiscSwapT0T1 $
        writeU64LE ssaAmountIn <>
        writeU64LE ssaMinOut

-- | Build instruction data for swapt1t0
swapT1T0Data :: SwapSimpleArgs -> ByteString
swapT1T0Data SwapSimpleArgs{..} =
    buildInstruction DiscSwapT1T0 $
        writeU64LE ssaAmountIn <>
        writeU64LE ssaMinOut

-- | Build instruction data for swapn (N-pool swap)
swapNData :: SwapNArgs -> ByteString
swapNData SwapNArgs{..} =
    buildInstruction DiscSwapN $
        writeU8 snaFromIdx <>
        writeU8 snaToIdx <>
        writeU64LE snaAmountIn <>
        writeU64LE snaMinOut

-- ============================================================================
-- Liquidity Instructions
-- ============================================================================

-- | Build instruction data for addliq
addLiquidityData :: AddLiqArgs -> ByteString
addLiquidityData AddLiqArgs{..} =
    buildInstruction DiscAddLiq $
        writeU64LE alaAmount0 <>
        writeU64LE alaAmount1 <>
        writeU64LE alaMinLp

-- | Build instruction data for addliq1 (single-sided)
addLiquidity1Data :: AddLiq1Args -> ByteString
addLiquidity1Data AddLiq1Args{..} =
    buildInstruction DiscAddLiq1 $
        writeU64LE al1aAmountIn <>
        writeU64LE al1aMinLp

-- | Build instruction data for remliq
removeLiquidityData :: RemLiqArgs -> ByteString
removeLiquidityData RemLiqArgs{..} =
    buildInstruction DiscRemLiq $
        writeU64LE rlaLpAmount <>
        writeU64LE rlaMin0 <>
        writeU64LE rlaMin1

-- ============================================================================
-- Admin Instructions
-- ============================================================================

-- | Build instruction data for setpause
setPauseData :: Bool -> ByteString
setPauseData paused =
    buildInstruction DiscSetPause $
        writeU8 (if paused then 1 else 0)

-- | Build instruction data for updfee
updateFeeData :: UpdateFeeArgs -> ByteString
updateFeeData UpdateFeeArgs{..} =
    buildInstruction DiscUpdateFee $
        writeU64LE ufaFeeBps

-- | Build instruction data for wdrawfee
withdrawFeeData :: ByteString
withdrawFeeData = discriminatorBytes DiscWithdrawFee

-- | Build instruction data for commitamp
commitAmpData :: CommitAmpArgs -> ByteString
commitAmpData CommitAmpArgs{..} =
    buildInstruction DiscCommitAmp $
        writeU64LE caaTargetAmp

-- | Build instruction data for rampamp
rampAmpData :: RampAmpArgs -> ByteString
rampAmpData RampAmpArgs{..} =
    buildInstruction DiscRampAmp $
        writeU64LE raaTargetAmp <>
        writeI64LE raaDuration

-- | Build instruction data for stopramp
stopRampData :: ByteString
stopRampData = discriminatorBytes DiscStopRamp

-- | Build instruction data for initauth
initAuthTransferData :: ByteString
initAuthTransferData = discriminatorBytes DiscInitAuth

-- | Build instruction data for complauth
completeAuthTransferData :: ByteString
completeAuthTransferData = discriminatorBytes DiscComplAuth

-- | Build instruction data for cancelauth
cancelAuthTransferData :: ByteString
cancelAuthTransferData = discriminatorBytes DiscCancelAuth

-- ============================================================================
-- Farming Instructions
-- ============================================================================

-- | Build instruction data for createfarm
createFarmData :: CreateFarmArgs -> ByteString
createFarmData CreateFarmArgs{..} =
    buildInstruction DiscCreateFarm $
        writeU64LE cfaRewardRate <>
        writeI64LE cfaStartTime <>
        writeI64LE cfaEndTime

-- | Build instruction data for stakelp
stakeLpData :: StakeArgs -> ByteString
stakeLpData StakeArgs{..} =
    buildInstruction DiscStakeLp $
        writeU64LE staAmount

-- | Build instruction data for unstakelp
unstakeLpData :: StakeArgs -> ByteString
unstakeLpData StakeArgs{..} =
    buildInstruction DiscUnstakeLp $
        writeU64LE staAmount

-- | Build instruction data for claimfarm
claimFarmData :: ByteString
claimFarmData = discriminatorBytes DiscClaimFarm

-- | Build instruction data for locklp
lockLpData :: LockLpArgs -> ByteString
lockLpData LockLpArgs{..} =
    buildInstruction DiscLockLp $
        writeU64LE llaAmount <>
        writeI64LE llaDuration

-- | Build instruction data for claimulp
claimUnlockedLpData :: ByteString
claimUnlockedLpData = discriminatorBytes DiscClaimUnlockedLp

-- ============================================================================
-- Lottery Instructions
-- ============================================================================

-- | Build instruction data for createlot
createLotteryData :: CreateLotteryArgs -> ByteString
createLotteryData CreateLotteryArgs{..} =
    buildInstruction DiscCreateLottery $
        writeU64LE claTicketPrice <>
        writeI64LE claEndTime

-- | Build instruction data for enterlot
enterLotteryData :: EnterLotteryArgs -> ByteString
enterLotteryData EnterLotteryArgs{..} =
    buildInstruction DiscEnterLottery $
        writeU64LE elaTicketCount

-- | Build instruction data for drawlot
drawLotteryData :: DrawLotteryArgs -> ByteString
drawLotteryData DrawLotteryArgs{..} =
    buildInstruction DiscDrawLottery $
        writeU64LE dlaRandomSeed

-- | Build instruction data for claimlot
claimLotteryData :: ByteString
claimLotteryData = discriminatorBytes DiscClaimLottery

-- ============================================================================
-- Oracle Instructions
-- ============================================================================

-- | Build instruction data for gettwap
getTwapData :: TwapWindow -> ByteString
getTwapData window =
    buildInstruction DiscGetTwap $
        writeU8 (fromIntegral $ fromEnum window)
