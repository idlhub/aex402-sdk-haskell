{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : AeX402.Types
-- Description : Data types for the AeX402 AMM program
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
--
-- This module defines all the data types used in the AeX402 AMM,
-- including pool states, farming, lottery, and instruction arguments.

module AeX402.Types
    ( -- * Solana Types
      Pubkey
    , mkPubkey
    , pubkeyBytes
    , pubkeyFromBytes
    , zeroPubkey

      -- * OHLCV Candle
    , Candle(..)
    , CandleDecoded(..)
    , decodeCandle

      -- * Pool State (2-token)
    , Pool(..)

      -- * N-Pool State (2-8 tokens)
    , NPool(..)

      -- * Lottery
    , Lottery(..)
    , LotteryEntry(..)

      -- * Farming
    , Farm(..)
    , UserFarm(..)

      -- * Registry
    , Registry(..)

      -- * TWAP Result
    , TwapResult(..)
    , decodeTwapResult
    , twapPriceAsFloat
    , twapConfidencePercent

      -- * Instruction Arguments
    , CreatePoolArgs(..)
    , CreateNPoolArgs(..)
    , SwapArgs(..)
    , SwapSimpleArgs(..)
    , SwapNArgs(..)
    , AddLiqArgs(..)
    , AddLiq1Args(..)
    , RemLiqArgs(..)
    , UpdateFeeArgs(..)
    , CommitAmpArgs(..)
    , RampAmpArgs(..)
    , CreateFarmArgs(..)
    , StakeArgs(..)
    , LockLpArgs(..)
    , CreateLotteryArgs(..)
    , EnterLotteryArgs(..)
    , DrawLotteryArgs(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int64)
import GHC.Generics (Generic)

-- ============================================================================
-- Solana Types
-- ============================================================================

-- | A Solana public key (32 bytes)
newtype Pubkey = Pubkey { pubkeyBytes :: ByteString }
    deriving (Eq, Ord, Generic)

instance Show Pubkey where
    show (Pubkey bs) = "Pubkey(" ++ show (BS.take 4 bs) ++ "...)"

-- | Create a Pubkey from bytes (must be exactly 32 bytes)
mkPubkey :: ByteString -> Maybe Pubkey
mkPubkey bs
    | BS.length bs == 32 = Just (Pubkey bs)
    | otherwise = Nothing

-- | Create a Pubkey from bytes, returns Nothing if wrong length
pubkeyFromBytes :: ByteString -> Maybe Pubkey
pubkeyFromBytes = mkPubkey

-- | The zero/null pubkey (32 zero bytes)
zeroPubkey :: Pubkey
zeroPubkey = Pubkey (BS.replicate 32 0)

-- ============================================================================
-- OHLCV Candle (12 bytes, delta-encoded)
-- ============================================================================

-- | Raw candle data as stored on-chain (delta-encoded)
data Candle = Candle
    { candleOpen   :: !Word32   -- ^ Base price (scaled 1e6)
    , candleHighD  :: !Word16   -- ^ High delta (high = open + highD)
    , candleLowD   :: !Word16   -- ^ Low delta (low = open - lowD)
    , candleCloseD :: !Int16    -- ^ Close delta signed (close = open + closeD)
    , candleVolume :: !Word16   -- ^ Volume in 1e9 units
    } deriving (Show, Eq, Generic)

-- | Decoded candle with absolute values
data CandleDecoded = CandleDecoded
    { cdOpen   :: !Word32
    , cdHigh   :: !Word32
    , cdLow    :: !Word32
    , cdClose  :: !Int64   -- ^ Can be negative if close < open - lowD
    , cdVolume :: !Word16
    } deriving (Show, Eq, Generic)

-- | Decode a delta-encoded candle to absolute values
decodeCandle :: Candle -> CandleDecoded
decodeCandle Candle{..} = CandleDecoded
    { cdOpen   = candleOpen
    , cdHigh   = candleOpen + fromIntegral candleHighD
    , cdLow    = candleOpen - fromIntegral candleLowD
    , cdClose  = fromIntegral candleOpen + fromIntegral candleCloseD
    , cdVolume = candleVolume
    }

-- ============================================================================
-- Pool State (2-token)
-- ============================================================================

-- | 2-token pool state (matches C struct, ~1024 bytes)
data Pool = Pool
    { poolDiscriminator :: !ByteString   -- ^ 8 bytes "POOLSWAP"
    , poolAuthority     :: !Pubkey       -- ^ Pool authority
    , poolMint0         :: !Pubkey       -- ^ Token 0 mint
    , poolMint1         :: !Pubkey       -- ^ Token 1 mint
    , poolVault0        :: !Pubkey       -- ^ Token 0 vault
    , poolVault1        :: !Pubkey       -- ^ Token 1 vault
    , poolLpMint        :: !Pubkey       -- ^ LP token mint
    , poolAmp           :: !Integer      -- ^ Current amplification
    , poolInitAmp       :: !Integer      -- ^ Initial amp for ramping
    , poolTargetAmp     :: !Integer      -- ^ Target amp for ramping
    , poolRampStart     :: !Integer      -- ^ Ramp start timestamp
    , poolRampStop      :: !Integer      -- ^ Ramp end timestamp
    , poolFeeBps        :: !Integer      -- ^ Swap fee in basis points
    , poolAdminFeePct   :: !Integer      -- ^ Admin fee percentage
    , poolBal0          :: !Integer      -- ^ Token 0 balance
    , poolBal1          :: !Integer      -- ^ Token 1 balance
    , poolLpSupply      :: !Integer      -- ^ LP token supply
    , poolAdminFee0     :: !Integer      -- ^ Accumulated admin fee token 0
    , poolAdminFee1     :: !Integer      -- ^ Accumulated admin fee token 1
    , poolVol0          :: !Integer      -- ^ Volume token 0
    , poolVol1          :: !Integer      -- ^ Volume token 1
    , poolPaused        :: !Bool         -- ^ Pool paused flag
    , poolBump          :: !Word8        -- ^ PDA bump seed
    , poolVault0Bump    :: !Word8        -- ^ Vault 0 bump
    , poolVault1Bump    :: !Word8        -- ^ Vault 1 bump
    , poolLpMintBump    :: !Word8        -- ^ LP mint bump
    , poolPendingAuth   :: !Pubkey       -- ^ Pending authority transfer
    , poolAuthTime      :: !Integer      -- ^ Authority transfer timestamp
    , poolPendingAmp    :: !Integer      -- ^ Pending amp change
    , poolAmpTime       :: !Integer      -- ^ Amp change timestamp
    -- Analytics
    , poolTradeCount    :: !Integer      -- ^ Total swap count
    , poolTradeSum      :: !Integer      -- ^ Sum of trade sizes
    , poolMaxPrice      :: !Word32       -- ^ All-time max price (scaled 1e6)
    , poolMinPrice      :: !Word32       -- ^ All-time min price (scaled 1e6)
    , poolHourSlot      :: !Word32       -- ^ Last updated hour slot
    , poolDaySlot       :: !Word32       -- ^ Last updated day slot
    , poolHourIdx       :: !Word8        -- ^ Current hour index 0-23
    , poolDayIdx        :: !Word8        -- ^ Current day index 0-6
    , poolBloom         :: !ByteString   -- ^ Bloom filter (128 bytes)
    , poolHourlyCandles :: ![Candle]     -- ^ 24 hourly candles
    , poolDailyCandles  :: ![Candle]     -- ^ 7 daily candles
    } deriving (Show, Eq, Generic)

-- ============================================================================
-- N-Pool State (2-8 tokens)
-- ============================================================================

-- | N-token pool state (up to 8 tokens, ~2048 bytes)
data NPool = NPool
    { nPoolDiscriminator :: !ByteString  -- ^ 8 bytes "NPOOLSWA"
    , nPoolAuthority     :: !Pubkey      -- ^ Pool authority
    , nPoolNTokens       :: !Word8       -- ^ Number of tokens (2-8)
    , nPoolPaused        :: !Bool        -- ^ Pool paused flag
    , nPoolBump          :: !Word8       -- ^ PDA bump seed
    , nPoolAmp           :: !Integer     -- ^ Amplification coefficient
    , nPoolFeeBps        :: !Integer     -- ^ Swap fee in basis points
    , nPoolAdminFeePct   :: !Integer     -- ^ Admin fee percentage
    , nPoolLpSupply      :: !Integer     -- ^ LP token supply
    , nPoolMints         :: ![Pubkey]    -- ^ Token mints (up to 8)
    , nPoolVaults        :: ![Pubkey]    -- ^ Token vaults (up to 8)
    , nPoolLpMint        :: !Pubkey      -- ^ LP token mint
    , nPoolBalances      :: ![Integer]   -- ^ Token balances (up to 8)
    , nPoolAdminFees     :: ![Integer]   -- ^ Admin fees per token (up to 8)
    , nPoolTotalVolume   :: !Integer     -- ^ Cumulative volume
    , nPoolTradeCount    :: !Integer     -- ^ Total number of swaps
    , nPoolLastTradeSlot :: !Integer     -- ^ Slot of last trade
    } deriving (Show, Eq, Generic)

-- ============================================================================
-- Lottery
-- ============================================================================

-- | Lottery state
data Lottery = Lottery
    { lotteryDiscriminator :: !ByteString
    , lotteryPool          :: !Pubkey     -- ^ Associated pool
    , lotteryAuthority     :: !Pubkey     -- ^ Lottery authority
    , lotteryVault         :: !Pubkey     -- ^ LP token vault
    , lotteryTicketPrice   :: !Integer    -- ^ LP tokens per ticket
    , lotteryTotalTickets  :: !Integer    -- ^ Total tickets sold
    , lotteryPrizePool     :: !Integer    -- ^ Prize in LP tokens
    , lotteryEndTime       :: !Integer    -- ^ End timestamp
    , lotteryWinningTicket :: !Integer    -- ^ Winning ticket number
    , lotteryDrawn         :: !Bool       -- ^ Has been drawn
    , lotteryClaimed       :: !Bool       -- ^ Prize claimed
    } deriving (Show, Eq, Generic)

-- | User lottery entry
data LotteryEntry = LotteryEntry
    { loteDiscriminator :: !ByteString
    , loteOwner         :: !Pubkey    -- ^ Entry owner
    , loteLottery       :: !Pubkey    -- ^ Lottery address
    , loteTicketStart   :: !Integer   -- ^ First ticket number
    , loteTicketCount   :: !Integer   -- ^ Number of tickets
    } deriving (Show, Eq, Generic)

-- ============================================================================
-- Farming
-- ============================================================================

-- | Farm state
data Farm = Farm
    { farmDiscriminator :: !ByteString
    , farmPool          :: !Pubkey    -- ^ Associated pool
    , farmRewardMint    :: !Pubkey    -- ^ Reward token mint
    , farmRewardRate    :: !Integer   -- ^ Reward per second
    , farmStartTime     :: !Integer   -- ^ Farm start timestamp
    , farmEndTime       :: !Integer   -- ^ Farm end timestamp
    , farmTotalStaked   :: !Integer   -- ^ Total LP staked
    , farmAccReward     :: !Integer   -- ^ Accumulated reward per share
    , farmLastUpdate    :: !Integer   -- ^ Last update timestamp
    } deriving (Show, Eq, Generic)

-- | User farm position
data UserFarm = UserFarm
    { ufarmDiscriminator :: !ByteString
    , ufarmOwner         :: !Pubkey   -- ^ Position owner
    , ufarmFarm          :: !Pubkey   -- ^ Farm address
    , ufarmStaked        :: !Integer  -- ^ Staked LP amount
    , ufarmRewardDebt    :: !Integer  -- ^ Reward debt for reward calc
    , ufarmLockEnd       :: !Integer  -- ^ Lock expiration timestamp
    } deriving (Show, Eq, Generic)

-- ============================================================================
-- Registry
-- ============================================================================

-- | Pool registry for enumeration
data Registry = Registry
    { regDiscriminator :: !ByteString
    , regAuthority     :: !Pubkey
    , regPendingAuth   :: !Pubkey
    , regAuthTime      :: !Integer
    , regCount         :: !Word32
    , regPools         :: ![Pubkey]
    } deriving (Show, Eq, Generic)

-- ============================================================================
-- TWAP Result
-- ============================================================================

-- | TWAP oracle result
data TwapResult = TwapResult
    { twapPrice      :: !Word32   -- ^ Price scaled 1e6
    , twapSamples    :: !Word16   -- ^ Number of candles used
    , twapConfidence :: !Word16   -- ^ Confidence 0-10000 (0-100%)
    } deriving (Show, Eq, Generic)

-- | Decode a TWAP result from the encoded u64 return value
decodeTwapResult :: Word64 -> TwapResult
decodeTwapResult val = TwapResult
    { twapPrice      = fromIntegral (val .&. 0xFFFFFFFF)
    , twapSamples    = fromIntegral ((val `shiftR` 32) .&. 0xFFFF)
    , twapConfidence = fromIntegral ((val `shiftR` 48) .&. 0xFFFF)
    }
  where
    (.&.) = (Prelude..&.)
    shiftR = Prelude.shiftR

-- | Get TWAP price as a floating point number
twapPriceAsFloat :: TwapResult -> Double
twapPriceAsFloat = (/ 1e6) . fromIntegral . twapPrice

-- | Get TWAP confidence as a percentage (0-100)
twapConfidencePercent :: TwapResult -> Double
twapConfidencePercent = (/ 100) . fromIntegral . twapConfidence

-- Helper for bit operations (avoiding Data.Bits import issues)
(.&.) :: Word64 -> Word64 -> Word64
(.&.) = \a b -> fromIntegral (fromIntegral a `bAnd` fromIntegral b :: Integer)
  where
    bAnd :: Integer -> Integer -> Integer
    bAnd x y = if x == 0 || y == 0 then 0 else
        let (qx, rx) = x `divMod` 2
            (qy, ry) = y `divMod` 2
        in (rx * ry) + 2 * bAnd qx qy

shiftR :: Word64 -> Int -> Word64
shiftR x n = fromIntegral (fromIntegral x `div` (2^n) :: Integer)

-- ============================================================================
-- Instruction Arguments
-- ============================================================================

-- | Arguments for createpool instruction
data CreatePoolArgs = CreatePoolArgs
    { cpaAmp  :: !Integer
    , cpaBump :: !Word8
    } deriving (Show, Eq, Generic)

-- | Arguments for createpn (N-pool) instruction
data CreateNPoolArgs = CreateNPoolArgs
    { cnpaAmp     :: !Integer
    , cnpaNTokens :: !Word8
    , cnpaBump    :: !Word8
    } deriving (Show, Eq, Generic)

-- | Arguments for generic swap instruction
data SwapArgs = SwapArgs
    { saFrom     :: !Word8     -- ^ Source token index
    , saTo       :: !Word8     -- ^ Destination token index
    , saAmountIn :: !Integer   -- ^ Input amount
    , saMinOut   :: !Integer   -- ^ Minimum output
    , saDeadline :: !Integer   -- ^ Deadline timestamp
    } deriving (Show, Eq, Generic)

-- | Arguments for simple swap (swapt0t1/swapt1t0)
data SwapSimpleArgs = SwapSimpleArgs
    { ssaAmountIn :: !Integer
    , ssaMinOut   :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for N-pool swap
data SwapNArgs = SwapNArgs
    { snaFromIdx  :: !Word8
    , snaToIdx    :: !Word8
    , snaAmountIn :: !Integer
    , snaMinOut   :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for adding liquidity
data AddLiqArgs = AddLiqArgs
    { alaAmount0 :: !Integer
    , alaAmount1 :: !Integer
    , alaMinLp   :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for single-sided liquidity add
data AddLiq1Args = AddLiq1Args
    { al1aAmountIn :: !Integer
    , al1aMinLp    :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for removing liquidity
data RemLiqArgs = RemLiqArgs
    { rlaLpAmount :: !Integer
    , rlaMin0     :: !Integer
    , rlaMin1     :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for updating fee
data UpdateFeeArgs = UpdateFeeArgs
    { ufaFeeBps :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for committing amp change
data CommitAmpArgs = CommitAmpArgs
    { caaTargetAmp :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for ramping amp
data RampAmpArgs = RampAmpArgs
    { raaTargetAmp :: !Integer
    , raaDuration  :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for creating farm
data CreateFarmArgs = CreateFarmArgs
    { cfaRewardRate :: !Integer
    , cfaStartTime  :: !Integer
    , cfaEndTime    :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for staking/unstaking LP
data StakeArgs = StakeArgs
    { staAmount :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for locking LP
data LockLpArgs = LockLpArgs
    { llaAmount   :: !Integer
    , llaDuration :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for creating lottery
data CreateLotteryArgs = CreateLotteryArgs
    { claTicketPrice :: !Integer
    , claEndTime     :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for entering lottery
data EnterLotteryArgs = EnterLotteryArgs
    { elaTicketCount :: !Integer
    } deriving (Show, Eq, Generic)

-- | Arguments for drawing lottery
data DrawLotteryArgs = DrawLotteryArgs
    { dlaRandomSeed :: !Integer
    } deriving (Show, Eq, Generic)
