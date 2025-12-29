{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AeX402.Accounts
-- Description : Account parsing for AeX402 AMM
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
--
-- This module provides functions for parsing binary account data
-- from the AeX402 AMM program into Haskell data structures.

module AeX402.Accounts
    ( -- * Pool Parsing
      parsePool
    , parseNPool

      -- * Farm Parsing
    , parseFarm
    , parseUserFarm

      -- * Lottery Parsing
    , parseLottery
    , parseLotteryEntry

      -- * Registry Parsing
    , parseRegistry

      -- * Validation
    , validatePoolDiscriminator
    , validateNPoolDiscriminator
    , validateFarmDiscriminator
    , validateLotteryDiscriminator
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int64)
import Data.Bits ((.&.), shiftR)

import AeX402.Constants
import AeX402.Types

-- ============================================================================
-- Binary Reading Helpers
-- ============================================================================

-- | Read a pubkey (32 bytes) at offset
readPubkey :: ByteString -> Int -> Maybe Pubkey
readPubkey bs offset
    | BS.length bs >= offset + 32 = mkPubkey (BS.take 32 $ BS.drop offset bs)
    | otherwise = Nothing

-- | Read an unsigned 8-bit integer at offset
readU8 :: ByteString -> Int -> Maybe Word8
readU8 bs offset
    | BS.length bs > offset = Just $ BS.index bs offset
    | otherwise = Nothing

-- | Read an unsigned 16-bit little-endian integer at offset
readU16LE :: ByteString -> Int -> Maybe Word16
readU16LE bs offset
    | BS.length bs >= offset + 2 =
        let b0 = fromIntegral $ BS.index bs offset
            b1 = fromIntegral $ BS.index bs (offset + 1)
        in Just $ b0 + (b1 `shiftL` 8)
    | otherwise = Nothing
  where
    shiftL :: Word16 -> Int -> Word16
    shiftL x n = x * (2 ^ n)

-- | Read a signed 16-bit little-endian integer at offset
readI16LE :: ByteString -> Int -> Maybe Int16
readI16LE bs offset = do
    u16 <- readU16LE bs offset
    return $ fromIntegral u16

-- | Read an unsigned 32-bit little-endian integer at offset
readU32LE :: ByteString -> Int -> Maybe Word32
readU32LE bs offset
    | BS.length bs >= offset + 4 =
        let b0 = fromIntegral $ BS.index bs offset
            b1 = fromIntegral $ BS.index bs (offset + 1)
            b2 = fromIntegral $ BS.index bs (offset + 2)
            b3 = fromIntegral $ BS.index bs (offset + 3)
        in Just $ b0 + (b1 `shiftL` 8) + (b2 `shiftL` 16) + (b3 `shiftL` 24)
    | otherwise = Nothing
  where
    shiftL :: Word32 -> Int -> Word32
    shiftL x n = x * (2 ^ n)

-- | Read an unsigned 64-bit little-endian integer at offset
readU64LE :: ByteString -> Int -> Maybe Integer
readU64LE bs offset
    | BS.length bs >= offset + 8 =
        let bytes = [BS.index bs (offset + i) | i <- [0..7]]
            vals = zipWith (\b i -> fromIntegral b * (256 ^ i)) bytes [0..]
        in Just $ sum vals
    | otherwise = Nothing

-- | Read a signed 64-bit little-endian integer at offset
readI64LE :: ByteString -> Int -> Maybe Integer
readI64LE = readU64LE  -- Same representation, interpret as signed in context

-- | Read a candle (12 bytes) at offset
readCandle :: ByteString -> Int -> Maybe Candle
readCandle bs offset = do
    open   <- readU32LE bs offset
    highD  <- readU16LE bs (offset + 4)
    lowD   <- readU16LE bs (offset + 6)
    closeD <- readI16LE bs (offset + 8)
    volume <- readU16LE bs (offset + 10)
    return Candle
        { candleOpen   = open
        , candleHighD  = highD
        , candleLowD   = lowD
        , candleCloseD = closeD
        , candleVolume = volume
        }

-- | Read multiple candles
readCandles :: ByteString -> Int -> Int -> Maybe [Candle]
readCandles bs offset count = sequence
    [readCandle bs (offset + i * 12) | i <- [0 .. count - 1]]

-- | Read multiple pubkeys
readPubkeys :: ByteString -> Int -> Int -> Maybe [Pubkey]
readPubkeys bs offset count = sequence
    [readPubkey bs (offset + i * 32) | i <- [0 .. count - 1]]

-- | Read multiple u64s as Integers
readU64s :: ByteString -> Int -> Int -> Maybe [Integer]
readU64s bs offset count = sequence
    [readU64LE bs (offset + i * 8) | i <- [0 .. count - 1]]

-- ============================================================================
-- Pool Parsing
-- ============================================================================

-- | Validate that the first 8 bytes match the Pool discriminator
validatePoolDiscriminator :: ByteString -> Bool
validatePoolDiscriminator bs =
    BS.length bs >= 8 && BS.take 8 bs == accountDiscriminatorBytes AccPool

-- | Parse a 2-token Pool from binary data
parsePool :: ByteString -> Maybe Pool
parsePool bs
    | BS.length bs < 900 = Nothing
    | not (validatePoolDiscriminator bs) = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        -- Pubkeys (6 * 32 = 192 bytes)
        authority <- readPubkey bs offset0
        mint0     <- readPubkey bs (offset0 + 32)
        mint1     <- readPubkey bs (offset0 + 64)
        vault0    <- readPubkey bs (offset0 + 96)
        vault1    <- readPubkey bs (offset0 + 128)
        lpMint    <- readPubkey bs (offset0 + 160)
        let offset1 = offset0 + 192

        -- Amp fields (5 * 8 = 40 bytes)
        amp       <- readU64LE bs offset1
        initAmp   <- readU64LE bs (offset1 + 8)
        targetAmp <- readU64LE bs (offset1 + 16)
        rampStart <- readI64LE bs (offset1 + 24)
        rampStop  <- readI64LE bs (offset1 + 32)
        let offset2 = offset1 + 40

        -- Fee fields (2 * 8 = 16 bytes)
        feeBps    <- readU64LE bs offset2
        adminPct  <- readU64LE bs (offset2 + 8)
        let offset3 = offset2 + 16

        -- Balance fields (5 * 8 = 40 bytes)
        bal0      <- readU64LE bs offset3
        bal1      <- readU64LE bs (offset3 + 8)
        lpSupply  <- readU64LE bs (offset3 + 16)
        adminFee0 <- readU64LE bs (offset3 + 24)
        adminFee1 <- readU64LE bs (offset3 + 32)
        let offset4 = offset3 + 40

        -- Volume fields (2 * 8 = 16 bytes)
        vol0 <- readU64LE bs offset4
        vol1 <- readU64LE bs (offset4 + 8)
        let offset5 = offset4 + 16

        -- Flags (5 bytes + 3 padding)
        pausedB      <- readU8 bs offset5
        bump         <- readU8 bs (offset5 + 1)
        vault0Bump   <- readU8 bs (offset5 + 2)
        vault1Bump   <- readU8 bs (offset5 + 3)
        lpMintBump   <- readU8 bs (offset5 + 4)
        let offset6 = offset5 + 8  -- Including 3 bytes padding

        -- Pending authority (32 + 8 = 40 bytes)
        pendingAuth <- readPubkey bs offset6
        authTime    <- readI64LE bs (offset6 + 32)
        let offset7 = offset6 + 40

        -- Pending amp (8 + 8 = 16 bytes)
        pendingAmp <- readU64LE bs offset7
        ampTime    <- readI64LE bs (offset7 + 8)
        let offset8 = offset7 + 16

        -- Analytics section
        tradeCount <- readU64LE bs offset8
        tradeSum   <- readU64LE bs (offset8 + 8)
        maxPrice   <- readU32LE bs (offset8 + 16)
        minPrice   <- readU32LE bs (offset8 + 20)
        hourSlot   <- readU32LE bs (offset8 + 24)
        daySlot    <- readU32LE bs (offset8 + 28)
        hourIdx    <- readU8 bs (offset8 + 32)
        dayIdx     <- readU8 bs (offset8 + 33)
        let offset9 = offset8 + 40  -- Including 6 bytes padding

        -- Bloom filter (128 bytes)
        let bloom = BS.take bloomSize $ BS.drop offset9 bs
        let offset10 = offset9 + bloomSize

        -- Candles
        hourlyCandles <- readCandles bs offset10 ohlcv24h
        let offset11 = offset10 + (ohlcv24h * 12)
        dailyCandles <- readCandles bs offset11 ohlcv7d

        return Pool
            { poolDiscriminator = disc
            , poolAuthority     = authority
            , poolMint0         = mint0
            , poolMint1         = mint1
            , poolVault0        = vault0
            , poolVault1        = vault1
            , poolLpMint        = lpMint
            , poolAmp           = amp
            , poolInitAmp       = initAmp
            , poolTargetAmp     = targetAmp
            , poolRampStart     = rampStart
            , poolRampStop      = rampStop
            , poolFeeBps        = feeBps
            , poolAdminFeePct   = adminPct
            , poolBal0          = bal0
            , poolBal1          = bal1
            , poolLpSupply      = lpSupply
            , poolAdminFee0     = adminFee0
            , poolAdminFee1     = adminFee1
            , poolVol0          = vol0
            , poolVol1          = vol1
            , poolPaused        = pausedB /= 0
            , poolBump          = bump
            , poolVault0Bump    = vault0Bump
            , poolVault1Bump    = vault1Bump
            , poolLpMintBump    = lpMintBump
            , poolPendingAuth   = pendingAuth
            , poolAuthTime      = authTime
            , poolPendingAmp    = pendingAmp
            , poolAmpTime       = ampTime
            , poolTradeCount    = tradeCount
            , poolTradeSum      = tradeSum
            , poolMaxPrice      = maxPrice
            , poolMinPrice      = minPrice
            , poolHourSlot      = hourSlot
            , poolDaySlot       = daySlot
            , poolHourIdx       = hourIdx
            , poolDayIdx        = dayIdx
            , poolBloom         = bloom
            , poolHourlyCandles = hourlyCandles
            , poolDailyCandles  = dailyCandles
            }

-- ============================================================================
-- N-Pool Parsing
-- ============================================================================

-- | Validate that the first 8 bytes match the NPool discriminator
validateNPoolDiscriminator :: ByteString -> Bool
validateNPoolDiscriminator bs =
    BS.length bs >= 8 && BS.take 8 bs == accountDiscriminatorBytes AccNPool

-- | Parse an N-token Pool from binary data
parseNPool :: ByteString -> Maybe NPool
parseNPool bs
    | BS.length bs < 800 = Nothing
    | not (validateNPoolDiscriminator bs) = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        authority <- readPubkey bs offset0
        let offset1 = offset0 + 32

        nTokens  <- readU8 bs offset1
        pausedB  <- readU8 bs (offset1 + 1)
        bump     <- readU8 bs (offset1 + 2)
        let offset2 = offset1 + 8  -- Including 5 bytes padding

        amp      <- readU64LE bs offset2
        feeBps   <- readU64LE bs (offset2 + 8)
        adminPct <- readU64LE bs (offset2 + 16)
        lpSupply <- readU64LE bs (offset2 + 24)
        let offset3 = offset2 + 32

        -- Mints (8 * 32 = 256 bytes)
        mints <- readPubkeys bs offset3 maxTokens
        let offset4 = offset3 + (maxTokens * 32)

        -- Vaults (8 * 32 = 256 bytes)
        vaults <- readPubkeys bs offset4 maxTokens
        let offset5 = offset4 + (maxTokens * 32)

        lpMint <- readPubkey bs offset5
        let offset6 = offset5 + 32

        -- Balances (8 * 8 = 64 bytes)
        balances <- readU64s bs offset6 maxTokens
        let offset7 = offset6 + (maxTokens * 8)

        -- Admin fees (8 * 8 = 64 bytes)
        adminFees <- readU64s bs offset7 maxTokens
        let offset8 = offset7 + (maxTokens * 8)

        totalVolume   <- readU64LE bs offset8
        tradeCount    <- readU64LE bs (offset8 + 8)
        lastTradeSlot <- readU64LE bs (offset8 + 16)

        return NPool
            { nPoolDiscriminator = disc
            , nPoolAuthority     = authority
            , nPoolNTokens       = nTokens
            , nPoolPaused        = pausedB /= 0
            , nPoolBump          = bump
            , nPoolAmp           = amp
            , nPoolFeeBps        = feeBps
            , nPoolAdminFeePct   = adminPct
            , nPoolLpSupply      = lpSupply
            , nPoolMints         = mints
            , nPoolVaults        = vaults
            , nPoolLpMint        = lpMint
            , nPoolBalances      = balances
            , nPoolAdminFees     = adminFees
            , nPoolTotalVolume   = totalVolume
            , nPoolTradeCount    = tradeCount
            , nPoolLastTradeSlot = lastTradeSlot
            }

-- ============================================================================
-- Farm Parsing
-- ============================================================================

-- | Validate that the first 8 bytes match the Farm discriminator
validateFarmDiscriminator :: ByteString -> Bool
validateFarmDiscriminator bs =
    BS.length bs >= 8 && BS.take 8 bs == accountDiscriminatorBytes AccFarm

-- | Parse a Farm from binary data
parseFarm :: ByteString -> Maybe Farm
parseFarm bs
    | BS.length bs < 120 = Nothing
    | not (validateFarmDiscriminator bs) = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        pool       <- readPubkey bs offset0
        rewardMint <- readPubkey bs (offset0 + 32)
        let offset1 = offset0 + 64

        rewardRate  <- readU64LE bs offset1
        startTime   <- readI64LE bs (offset1 + 8)
        endTime     <- readI64LE bs (offset1 + 16)
        totalStaked <- readU64LE bs (offset1 + 24)
        accReward   <- readU64LE bs (offset1 + 32)
        lastUpdate  <- readI64LE bs (offset1 + 40)

        return Farm
            { farmDiscriminator = disc
            , farmPool          = pool
            , farmRewardMint    = rewardMint
            , farmRewardRate    = rewardRate
            , farmStartTime     = startTime
            , farmEndTime       = endTime
            , farmTotalStaked   = totalStaked
            , farmAccReward     = accReward
            , farmLastUpdate    = lastUpdate
            }

-- | Parse a UserFarm from binary data
parseUserFarm :: ByteString -> Maybe UserFarm
parseUserFarm bs
    | BS.length bs < 96 = Nothing
    | BS.take 8 bs /= accountDiscriminatorBytes AccUserFarm = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        owner      <- readPubkey bs offset0
        farm       <- readPubkey bs (offset0 + 32)
        let offset1 = offset0 + 64

        staked     <- readU64LE bs offset1
        rewardDebt <- readU64LE bs (offset1 + 8)
        lockEnd    <- readI64LE bs (offset1 + 16)

        return UserFarm
            { ufarmDiscriminator = disc
            , ufarmOwner         = owner
            , ufarmFarm          = farm
            , ufarmStaked        = staked
            , ufarmRewardDebt    = rewardDebt
            , ufarmLockEnd       = lockEnd
            }

-- ============================================================================
-- Lottery Parsing
-- ============================================================================

-- | Validate that the first 8 bytes match the Lottery discriminator
validateLotteryDiscriminator :: ByteString -> Bool
validateLotteryDiscriminator bs =
    BS.length bs >= 8 && BS.take 8 bs == accountDiscriminatorBytes AccLottery

-- | Parse a Lottery from binary data
parseLottery :: ByteString -> Maybe Lottery
parseLottery bs
    | BS.length bs < 152 = Nothing
    | not (validateLotteryDiscriminator bs) = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        pool         <- readPubkey bs offset0
        authority    <- readPubkey bs (offset0 + 32)
        lotteryVault <- readPubkey bs (offset0 + 64)
        let offset1 = offset0 + 96

        ticketPrice   <- readU64LE bs offset1
        totalTickets  <- readU64LE bs (offset1 + 8)
        prizePool     <- readU64LE bs (offset1 + 16)
        endTime       <- readI64LE bs (offset1 + 24)
        winningTicket <- readU64LE bs (offset1 + 32)
        let offset2 = offset1 + 40

        drawnB   <- readU8 bs offset2
        claimedB <- readU8 bs (offset2 + 1)

        return Lottery
            { lotteryDiscriminator = disc
            , lotteryPool          = pool
            , lotteryAuthority     = authority
            , lotteryVault         = lotteryVault
            , lotteryTicketPrice   = ticketPrice
            , lotteryTotalTickets  = totalTickets
            , lotteryPrizePool     = prizePool
            , lotteryEndTime       = endTime
            , lotteryWinningTicket = winningTicket
            , lotteryDrawn         = drawnB /= 0
            , lotteryClaimed       = claimedB /= 0
            }

-- | Parse a LotteryEntry from binary data
parseLotteryEntry :: ByteString -> Maybe LotteryEntry
parseLotteryEntry bs
    | BS.length bs < 88 = Nothing
    | BS.take 8 bs /= accountDiscriminatorBytes AccLotEntry = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        owner       <- readPubkey bs offset0
        lottery     <- readPubkey bs (offset0 + 32)
        let offset1 = offset0 + 64

        ticketStart <- readU64LE bs offset1
        ticketCount <- readU64LE bs (offset1 + 8)

        return LotteryEntry
            { loteDiscriminator = disc
            , loteOwner         = owner
            , loteLottery       = lottery
            , loteTicketStart   = ticketStart
            , loteTicketCount   = ticketCount
            }

-- ============================================================================
-- Registry Parsing
-- ============================================================================

-- | Parse a Registry from binary data
parseRegistry :: ByteString -> Maybe Registry
parseRegistry bs
    | BS.length bs < 88 = Nothing
    | BS.take 8 bs /= accountDiscriminatorBytes AccRegistry = Nothing
    | otherwise = do
        let disc = BS.take 8 bs
        let offset0 = 8

        authority   <- readPubkey bs offset0
        pendingAuth <- readPubkey bs (offset0 + 32)
        let offset1 = offset0 + 64

        authTime <- readI64LE bs offset1
        count    <- readU32LE bs (offset1 + 8)
        let offset2 = offset1 + 12

        -- Read pool addresses based on count
        let poolCount = min (fromIntegral count) 256  -- Reasonable limit
        pools <- readPubkeys bs offset2 poolCount

        return Registry
            { regDiscriminator = disc
            , regAuthority     = authority
            , regPendingAuth   = pendingAuth
            , regAuthTime      = authTime
            , regCount         = count
            , regPools         = pools
            }
