# AeX402 Haskell SDK

A pure Haskell SDK for interacting with the AeX402 hybrid AMM program on Solana.

## Program ID

```
3AMM53MsJZy2Jvf7PeHHga3bsGjWV4TSaYz29WUtcdje
```

## Features

- **Dual Pool Types**: Stable pools (high amp) and volatile pools (amp=1)
- **N-Token Pools**: Support for 2-8 token pools with AeX402 StableSwap math
- **Newton's Method**: Precise swap simulation using iterative numerical methods
- **Binary Parsing**: Full account deserialization for Pool, NPool, Farm, Lottery, etc.
- **Instruction Building**: Build instruction data for all 55+ handlers
- **PDA Derivation**: Compute Program Derived Addresses for all account types

## Installation

Add to your `cabal.project` or `stack.yaml`:

```cabal
-- cabal.project
packages: .
          path/to/aex402-sdk
```

Or add as a dependency in your `.cabal` file:

```cabal
build-depends:
    aex402-sdk >= 0.1.0.0
```

## Quick Start

```haskell
import AeX402

-- Simulate a swap
main :: IO ()
main = do
    let result = simulateSwap
            1000000000  -- bal0 (1 token with 9 decimals)
            1000000000  -- bal1
            10000000    -- amountIn (0.01 tokens)
            100         -- amp (stable pool)
            30          -- feeBps (0.30%)

    case result of
        Just amountOut -> putStrLn $ "Output: " ++ show amountOut
        Nothing -> putStrLn "Swap simulation failed"
```

## Modules

### AeX402.Constants

Program constants including:
- `programId` - The program ID string
- `Discriminator` - All 65+ instruction discriminators
- `AccountDiscriminator` - Account type markers
- `AeX402Error` - Error codes with messages
- Pool, farming, governance, and ML brain constants

### AeX402.Types

Data types for all on-chain structures:
- `Pool` - 2-token pool state (1024 bytes)
- `NPool` - N-token pool state (2048 bytes)
- `Farm` / `UserFarm` - Farming state
- `Lottery` / `LotteryEntry` - Lottery state
- `Candle` - OHLCV analytics data
- Instruction argument types

### AeX402.Accounts

Binary account parsing:
- `parsePool` - Parse Pool from bytes
- `parseNPool` - Parse N-Pool from bytes
- `parseFarm` / `parseUserFarm` - Parse farming accounts
- `parseLottery` / `parseLotteryEntry` - Parse lottery accounts
- Discriminator validation functions

### AeX402.Instructions

Instruction data builders:
- `createPoolData` - Pool creation
- `swapData` / `swapT0T1Data` - Swap instructions
- `addLiquidityData` / `removeLiquidityData` - Liquidity operations
- Admin, farming, lottery instructions

### AeX402.Math

StableSwap mathematics:
- `calcD` - Calculate invariant D for 2-token pool
- `calcY` - Calculate output amount
- `calcDN` - Calculate invariant for N-token pool
- `simulateSwap` - Full swap simulation with fees
- `calcLpTokens` / `calcWithdraw` - LP token math
- `getCurrentAmp` - Amp ramping interpolation
- `isqrt` - Integer square root

### AeX402.PDA

PDA derivation utilities:
- `derivePoolPda` / `deriveNPoolPda`
- `deriveVault0Pda` / `deriveVault1Pda` / `deriveLpMintPda`
- `deriveFarmPda` / `deriveUserFarmPda`
- `deriveLotteryPda` / `deriveLotteryEntryPda`
- `deriveRegistryPda`

## Building

```bash
# Using cabal
cabal build

# Using stack
stack build

# Run tests
cabal test
# or
stack test
```

## Example: Calculate LP Tokens for Deposit

```haskell
import AeX402

calculateDeposit :: IO ()
calculateDeposit = do
    let lpTokens = calcLpTokens
            1000000000   -- amt0 to deposit
            1000000000   -- amt1 to deposit
            10000000000  -- current bal0
            10000000000  -- current bal1
            9000000000   -- current LP supply
            100          -- amp

    case lpTokens of
        Just lp -> putStrLn $ "Will receive " ++ show lp ++ " LP tokens"
        Nothing -> putStrLn "Calculation failed"
```

## Example: Parse Pool Account

```haskell
import AeX402
import qualified Data.ByteString as BS

parsePoolAccount :: ByteString -> IO ()
parsePoolAccount accountData = do
    case parsePool accountData of
        Just pool -> do
            putStrLn $ "Pool amp: " ++ show (poolAmp pool)
            putStrLn $ "Balance 0: " ++ show (poolBal0 pool)
            putStrLn $ "Balance 1: " ++ show (poolBal1 pool)
            putStrLn $ "LP Supply: " ++ show (poolLpSupply pool)
            putStrLn $ "Fee: " ++ show (poolFeeBps pool) ++ " bps"
        Nothing -> putStrLn "Failed to parse pool"
```

## Example: Build Swap Instruction

```haskell
import AeX402
import qualified Data.ByteString as BS

buildSwapIx :: IO ByteString
buildSwapIx = do
    let args = SwapSimpleArgs
            { ssaAmountIn = 1000000000   -- 1 token
            , ssaMinOut   = 990000000    -- 0.99 tokens (1% slippage)
            }
    return $ swapT0T1Data args
```

## Math Details

The SDK implements the AeX402 StableSwap curve (also known as Curve's StableSwap invariant):

```
A * n^n * sum(x_i) + D = A * D * n^n + D^(n+1) / (n^n * prod(x_i))
```

For a 2-token pool (n=2):
```
4A(x+y) + D = 4AD + D^3 / (4xy)
```

Newton's method iterates to find D (invariant) and y (output balance) with a maximum of 255 iterations for convergence.

## License

MIT License - see [LICENSE](LICENSE)
