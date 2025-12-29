{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : AeX402.Math
-- Description : StableSwap math for AeX402 AMM
-- Copyright   : (c) 2024 AeX402 Team
-- License     : MIT
--
-- This module provides pure Haskell implementations of the StableSwap
-- mathematical functions using Newton's method iteration. All calculations
-- use Integer for arbitrary precision to match the on-chain behavior.

module AeX402.Math
    ( -- * Core StableSwap Math
      calcD
    , calcY
    , calcDN

      -- * Swap Simulation
    , simulateSwap
    , simulateSwapN
    , calcPriceImpact

      -- * Liquidity Math
    , calcLpTokens
    , calcWithdraw
    , calcVirtualPrice

      -- * Amp Ramping
    , getCurrentAmp

      -- * Utility Functions
    , calcMinOutput
    , checkImbalance
    , isqrt
    ) where

import AeX402.Constants (newtonIterations)

-- ============================================================================
-- Constants
-- ============================================================================

-- | Fee denominator (basis points)
feeDenominator :: Integer
feeDenominator = 10000

-- ============================================================================
-- Core StableSwap Math
-- ============================================================================

-- | Calculate invariant D for a 2-token pool using Newton's method.
--
-- The invariant equation is:
-- A*n^n * sum(x_i) + D = A*D*n^n + D^(n+1) / (n^n * prod(x_i))
--
-- For n=2:
-- 4*A*(x+y) + D = 4*A*D + D^3 / (4*x*y)
--
-- Returns Nothing if Newton's method fails to converge.
calcD
    :: Integer  -- ^ Balance x
    -> Integer  -- ^ Balance y
    -> Integer  -- ^ Amplification coefficient
    -> Maybe Integer
calcD x y amp
    | s == 0 = Just 0
    | otherwise = newton 0 s
  where
    s = x + y
    ann = amp * 4  -- A * n^n where n=2

    newton :: Int -> Integer -> Maybe Integer
    newton !iter !d
        | iter >= newtonIterations = Nothing
        | otherwise =
            let
                -- d_p = D^3 / (4 * x * y)
                dP = (d * d `div` (x * 2)) * d `div` (y * 2)

                -- d_new = (ann * s + d_p * 2) * d / ((ann - 1) * d + 3 * d_p)
                num = (ann * s + dP * 2) * d
                denom = (ann - 1) * d + dP * 3
                dNew = num `div` denom
            in
                if abs (dNew - d) <= 1
                    then Just dNew
                    else newton (iter + 1) dNew

-- | Calculate output amount y given new input balance.
--
-- Solves for y in the StableSwap invariant equation given:
-- - The new balance of the input token (xNew)
-- - The invariant D
-- - The amplification coefficient
--
-- Returns Nothing if Newton's method fails to converge.
calcY
    :: Integer  -- ^ New balance of input token
    -> Integer  -- ^ Invariant D
    -> Integer  -- ^ Amplification coefficient
    -> Maybe Integer
calcY xNew d amp
    | xNew <= 0 || d <= 0 = Nothing
    | otherwise = newton 0 d
  where
    ann = amp * 4

    -- c = D^3 / (4 * x_new * A*n^n)
    c = (d * d `div` (xNew * 2)) * d `div` (ann * 2)

    -- b = x_new + D / ann
    b = xNew + d `div` ann

    newton :: Int -> Integer -> Maybe Integer
    newton !iter !y
        | iter >= newtonIterations = Nothing
        | otherwise =
            let
                -- y_new = (y^2 + c) / (2*y + b - D)
                num = y * y + c
                denom = 2 * y + b - d
                yNew = num `div` denom
            in
                if abs (yNew - y) <= 1
                    then Just yNew
                    else newton (iter + 1) yNew

-- | Calculate invariant D for an N-token pool using Newton's method.
--
-- Generalized form for n tokens:
-- A*n^n * sum(x_i) + D = A*D*n^n + D^(n+1) / (n^n * prod(x_i))
--
-- Returns Nothing if Newton's method fails to converge.
calcDN
    :: [Integer]  -- ^ Token balances
    -> Integer    -- ^ Amplification coefficient
    -> Maybe Integer
calcDN balances amp
    | null balances = Nothing
    | s == 0 = Just 0
    | otherwise = newton 0 s
  where
    n = fromIntegral (length balances) :: Integer
    s = sum balances
    ann = amp * (n ^ n)  -- A * n^n

    -- Product of all balances scaled by n^n
    balProduct = product balances * (n ^ n)

    newton :: Int -> Integer -> Maybe Integer
    newton !iter !d
        | iter >= newtonIterations = Nothing
        | balProduct == 0 = Nothing
        | otherwise =
            let
                -- d_p = D^(n+1) / (n^n * prod(x_i))
                dP = foldr (\b acc -> acc * d `div` (b * n)) d balances

                -- d_new = (ann * s + n * d_p) * d / ((ann - 1) * d + (n+1) * d_p)
                num = (ann * s + n * dP) * d
                denom = (ann - 1) * d + (n + 1) * dP
                dNew = num `div` denom
            in
                if abs (dNew - d) <= 1
                    then Just dNew
                    else newton (iter + 1) dNew

-- ============================================================================
-- Swap Simulation
-- ============================================================================

-- | Simulate a swap and return the output amount after fees.
--
-- Returns Nothing if the math fails to converge.
simulateSwap
    :: Integer  -- ^ Input token balance
    -> Integer  -- ^ Output token balance
    -> Integer  -- ^ Amount to swap in
    -> Integer  -- ^ Amplification coefficient
    -> Integer  -- ^ Fee in basis points
    -> Maybe Integer
simulateSwap balIn balOut amountIn amp feeBps = do
    d <- calcD balIn balOut amp
    let newBalIn = balIn + amountIn
    newBalOut <- calcY newBalIn d amp
    let amountOut = balOut - newBalOut
    let fee = amountOut * feeBps `div` feeDenominator
    return (amountOut - fee)

-- | Simulate an N-pool swap and return the output amount after fees.
simulateSwapN
    :: [Integer]  -- ^ All token balances
    -> Int        -- ^ Input token index
    -> Int        -- ^ Output token index
    -> Integer    -- ^ Amount to swap in
    -> Integer    -- ^ Amplification coefficient
    -> Integer    -- ^ Fee in basis points
    -> Maybe Integer
simulateSwapN balances fromIdx toIdx amountIn amp feeBps
    | fromIdx < 0 || fromIdx >= length balances = Nothing
    | toIdx < 0 || toIdx >= length balances = Nothing
    | fromIdx == toIdx = Nothing
    | otherwise = do
        d <- calcDN balances amp
        let newBalances = updateAt fromIdx (+ amountIn) balances
        let balOut = balances !! toIdx
        -- For N-pool, we need to solve for the new output balance
        -- This is a simplified approach - proper implementation would use calcYN
        newD <- calcDN newBalances amp
        -- Approximate output using the change in invariant
        let amountOut = if newD >= d
                then balOut * (newD - d) `div` d
                else 0
        let fee = amountOut * feeBps `div` feeDenominator
        return (amountOut - fee)
  where
    updateAt :: Int -> (Integer -> Integer) -> [Integer] -> [Integer]
    updateAt idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

-- | Calculate price impact for a swap (as a fraction, e.g., 0.01 = 1%)
calcPriceImpact
    :: Integer  -- ^ Input token balance
    -> Integer  -- ^ Output token balance
    -> Integer  -- ^ Amount to swap in
    -> Integer  -- ^ Amplification coefficient
    -> Integer  -- ^ Fee in basis points
    -> Maybe Double
calcPriceImpact balIn balOut amountIn amp feeBps = do
    amountOut <- simulateSwap balIn balOut amountIn amp feeBps
    -- Price impact = 1 - (amountOut / amountIn)
    let ratio = fromIntegral amountOut / fromIntegral amountIn :: Double
    return (1 - ratio)

-- ============================================================================
-- Liquidity Math
-- ============================================================================

-- | Calculate LP tokens to mint for a deposit.
--
-- For initial deposit, returns sqrt(amt0 * amt1).
-- For subsequent deposits, returns lp_supply * (d1 - d0) / d0.
calcLpTokens
    :: Integer  -- ^ Amount of token 0 to deposit
    -> Integer  -- ^ Amount of token 1 to deposit
    -> Integer  -- ^ Current balance of token 0
    -> Integer  -- ^ Current balance of token 1
    -> Integer  -- ^ Current LP supply
    -> Integer  -- ^ Amplification coefficient
    -> Maybe Integer
calcLpTokens amt0 amt1 bal0 bal1 lpSupply amp
    | lpSupply == 0 =
        -- Initial deposit: LP = sqrt(amt0 * amt1)
        Just $ isqrt (amt0 * amt1)
    | otherwise = do
        d0 <- calcD bal0 bal1 amp
        d1 <- calcD (bal0 + amt0) (bal1 + amt1) amp
        if d0 == 0
            then Nothing
            else Just $ lpSupply * (d1 - d0) `div` d0

-- | Calculate tokens received for burning LP tokens.
--
-- Returns proportional amounts of both tokens.
calcWithdraw
    :: Integer  -- ^ LP amount to burn
    -> Integer  -- ^ Current balance of token 0
    -> Integer  -- ^ Current balance of token 1
    -> Integer  -- ^ Current LP supply
    -> Maybe (Integer, Integer)
calcWithdraw lpAmount bal0 bal1 lpSupply
    | lpSupply == 0 = Nothing
    | otherwise =
        let amount0 = bal0 * lpAmount `div` lpSupply
            amount1 = bal1 * lpAmount `div` lpSupply
        in Just (amount0, amount1)

-- | Calculate the virtual price (LP value relative to underlying).
--
-- Virtual price = D * 1e18 / lpSupply
calcVirtualPrice
    :: Integer  -- ^ Balance of token 0
    -> Integer  -- ^ Balance of token 1
    -> Integer  -- ^ LP supply
    -> Integer  -- ^ Amplification coefficient
    -> Maybe Integer
calcVirtualPrice bal0 bal1 lpSupply amp
    | lpSupply == 0 = Nothing
    | otherwise = do
        d <- calcD bal0 bal1 amp
        let precision = 10 ^ (18 :: Integer)
        return $ d * precision `div` lpSupply

-- ============================================================================
-- Amp Ramping
-- ============================================================================

-- | Calculate the current amplification during a ramp.
--
-- Linearly interpolates between initial and target amp based on elapsed time.
getCurrentAmp
    :: Integer  -- ^ Initial/current amp
    -> Integer  -- ^ Target amp
    -> Integer  -- ^ Ramp start timestamp
    -> Integer  -- ^ Ramp end timestamp
    -> Integer  -- ^ Current timestamp
    -> Integer
getCurrentAmp amp targetAmp rampStart rampEnd now
    | now >= rampEnd || rampEnd == rampStart = targetAmp
    | now <= rampStart = amp
    | otherwise =
        let elapsed = now - rampStart
            duration = rampEnd - rampStart
        in if targetAmp > amp
            then amp + (targetAmp - amp) * elapsed `div` duration
            else amp - (amp - targetAmp) * elapsed `div` duration

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Calculate minimum output with slippage tolerance.
calcMinOutput
    :: Integer  -- ^ Expected output
    -> Integer  -- ^ Slippage in basis points
    -> Integer
calcMinOutput expectedOutput slippageBps =
    expectedOutput * (feeDenominator - slippageBps) `div` feeDenominator

-- | Check if pool imbalance is within acceptable range.
checkImbalance
    :: Integer  -- ^ Balance of token 0
    -> Integer  -- ^ Balance of token 1
    -> Integer  -- ^ Maximum allowed ratio (e.g., 10 = 10:1)
    -> Bool
checkImbalance bal0 bal1 maxRatio
    | bal0 == 0 || bal1 == 0 = False
    | otherwise =
        let ratio = if bal0 > bal1
                then (bal0 * 100) `div` bal1
                else (bal1 * 100) `div` bal0
        in ratio <= maxRatio * 100

-- | Integer square root using Newton's method.
isqrt :: Integer -> Integer
isqrt n
    | n <= 0 = 0
    | n <= 3 = 1
    | otherwise = newton (n `div` 2)
  where
    newton !x =
        let x' = (x + n `div` x) `div` 2
        in if x' >= x then x else newton x'
