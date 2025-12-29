{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString as BS
import Data.Maybe (isJust, isNothing)

import AeX402

main :: IO ()
main = hspec $ do
    describe "AeX402.Math" $ do
        describe "calcD" $ do
            it "returns 0 for zero balances" $ do
                calcD 0 0 100 `shouldBe` Just 0

            it "converges for balanced pool" $ do
                let result = calcD 1000000 1000000 100
                result `shouldSatisfy` isJust

            it "converges for imbalanced pool" $ do
                let result = calcD 1000000 500000 100
                result `shouldSatisfy` isJust

            it "returns approximately sum for high amp" $ do
                let result = calcD 1000000 1000000 10000
                case result of
                    Just d -> d `shouldSatisfy` (\x -> x > 1900000 && x < 2100000)
                    Nothing -> expectationFailure "Failed to converge"

        describe "calcY" $ do
            it "calculates correct output for swap" $ do
                let mD = calcD 1000000 1000000 100
                case mD of
                    Just d -> do
                        let mY = calcY 1100000 d 100
                        mY `shouldSatisfy` isJust
                        case mY of
                            Just y -> y `shouldSatisfy` (< 1000000)
                            Nothing -> expectationFailure "calcY failed"
                    Nothing -> expectationFailure "calcD failed"

        describe "simulateSwap" $ do
            it "returns output less than input for stable pool" $ do
                let result = simulateSwap 1000000000 1000000000 10000000 100 30
                case result of
                    Just out -> out `shouldSatisfy` (< 10000000)
                    Nothing -> expectationFailure "Swap simulation failed"

            it "applies fee correctly" $ do
                let noFee = simulateSwap 1000000000 1000000000 10000000 100 0
                let withFee = simulateSwap 1000000000 1000000000 10000000 100 30
                case (noFee, withFee) of
                    (Just n, Just w) -> n `shouldSatisfy` (> w)
                    _ -> expectationFailure "Swap simulation failed"

        describe "calcLpTokens" $ do
            it "calculates sqrt for initial deposit" $ do
                let result = calcLpTokens 1000000 1000000 0 0 0 100
                result `shouldBe` Just 1000000

            it "calculates proportional LP for subsequent deposits" $ do
                let result = calcLpTokens 100000 100000 1000000 1000000 1000000 100
                result `shouldSatisfy` isJust

        describe "isqrt" $ do
            it "calculates correct square roots" $ do
                isqrt 0 `shouldBe` 0
                isqrt 1 `shouldBe` 1
                isqrt 4 `shouldBe` 2
                isqrt 9 `shouldBe` 3
                isqrt 100 `shouldBe` 10
                isqrt 1000000 `shouldBe` 1000

            it "rounds down for non-perfect squares" $ do
                isqrt 5 `shouldBe` 2
                isqrt 10 `shouldBe` 3

        describe "getCurrentAmp" $ do
            it "returns target amp after ramp ends" $ do
                getCurrentAmp 100 200 1000 2000 3000 `shouldBe` 200

            it "returns initial amp before ramp starts" $ do
                getCurrentAmp 100 200 1000 2000 500 `shouldBe` 100

            it "interpolates during ramp" $ do
                getCurrentAmp 100 200 1000 2000 1500 `shouldBe` 150

    describe "AeX402.Instructions" $ do
        describe "discriminators" $ do
            it "all discriminators are 8 bytes" $ do
                BS.length (discriminatorBytes DiscCreatePool) `shouldBe` 8
                BS.length (discriminatorBytes DiscSwap) `shouldBe` 8
                BS.length (discriminatorBytes DiscAddLiq) `shouldBe` 8

        describe "instruction builders" $ do
            it "createPoolData has correct length" $ do
                let args = CreatePoolArgs { cpaAmp = 100, cpaBump = 255 }
                BS.length (createPoolData args) `shouldBe` 17  -- 8 + 8 + 1

            it "swapT0T1Data has correct length" $ do
                let args = SwapSimpleArgs { ssaAmountIn = 1000000, ssaMinOut = 900000 }
                BS.length (swapT0T1Data args) `shouldBe` 24  -- 8 + 8 + 8

            it "addLiquidityData has correct length" $ do
                let args = AddLiqArgs
                        { alaAmount0 = 1000000
                        , alaAmount1 = 1000000
                        , alaMinLp = 900000
                        }
                BS.length (addLiquidityData args) `shouldBe` 32  -- 8 + 8 + 8 + 8

    describe "AeX402.Accounts" $ do
        describe "discriminator validation" $ do
            it "validates Pool discriminator" $ do
                validatePoolDiscriminator "POOLSWAP........" `shouldBe` True
                validatePoolDiscriminator "NOTPOOL!........" `shouldBe` False
                validatePoolDiscriminator "SHORT" `shouldBe` False

            it "validates NPool discriminator" $ do
                validateNPoolDiscriminator "NPOOLSWA........" `shouldBe` True
                validateNPoolDiscriminator "POOLSWAP........" `shouldBe` False

            it "validates Farm discriminator" $ do
                validateFarmDiscriminator "FARMSWAP........" `shouldBe` True

            it "validates Lottery discriminator" $ do
                validateLotteryDiscriminator "LOTTERY!........" `shouldBe` True

    describe "AeX402.Types" $ do
        describe "Pubkey" $ do
            it "creates pubkey from 32 bytes" $ do
                let bytes = BS.replicate 32 0
                mkPubkey bytes `shouldSatisfy` isJust

            it "rejects non-32-byte input" $ do
                mkPubkey "short" `shouldBe` Nothing
                mkPubkey (BS.replicate 33 0) `shouldBe` Nothing

        describe "Candle decoding" $ do
            it "decodes candle correctly" $ do
                let candle = Candle
                        { candleOpen = 1000000
                        , candleHighD = 10000
                        , candleLowD = 5000
                        , candleCloseD = 2000
                        , candleVolume = 100
                        }
                let decoded = decodeCandle candle
                cdOpen decoded `shouldBe` 1000000
                cdHigh decoded `shouldBe` 1010000
                cdLow decoded `shouldBe` 995000
                cdClose decoded `shouldBe` 1002000
                cdVolume decoded `shouldBe` 100

    describe "AeX402.Constants" $ do
        it "has correct program ID" $ do
            programId `shouldBe` "3AMM53MsJZy2Jvf7PeHHga3bsGjWV4TSaYz29WUtcdje"

        it "has valid amp bounds" $ do
            minAmp `shouldBe` 1
            maxAmp `shouldBe` 100000
            minAmp `shouldSatisfy` (< maxAmp)

        it "has correct pool sizes" $ do
            poolSize `shouldBe` 1024
            nPoolSize `shouldBe` 2048

        it "has correct analytics constants" $ do
            ohlcv24h `shouldBe` 24
            ohlcv7d `shouldBe` 7
            bloomSize `shouldBe` 128

    describe "Error codes" $ do
        it "maps error codes correctly" $ do
            errorCode ErrPaused `shouldBe` 6000
            errorCode ErrInvalidAmp `shouldBe` 6001
            errorCode ErrDurationError `shouldBe` 6030

        it "provides meaningful error messages" $ do
            errorMessage ErrPaused `shouldBe` "Pool is paused"
            errorMessage ErrSlippage `shouldBe` "Slippage exceeded"
