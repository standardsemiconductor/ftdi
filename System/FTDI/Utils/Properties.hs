{-# LANGUAGE ScopedTypeVariables #-}

module System.FTDI.Utils.Properties where

import System.FTDI.Utils ( clamp, divRndUp )
import Test.QuickCheck ( Property, (==>) )

prop_divRndUp_min :: Integral a => a -> a -> Property
prop_divRndUp_min x y = y /= 0 ==>
    let d  = divRndUp x (abs y)
        d' = toInteger d
        y' = toInteger y
        x' = toInteger x
    in d' * abs y' >= x'

prop_divRndUp_max :: Integral a => a -> a -> Property
prop_divRndUp_max x y = y /= 0 ==>
    let d = divRndUp x y
    in x `div` y <= d

prop_divRndUp_ceilFrac :: Integral a => a -> a -> Property
prop_divRndUp_ceilFrac x y = y /= 0 ==>
    let x' = fromIntegral x :: Double
        y' = fromIntegral y :: Double
    in divRndUp x y == ceilFrac x' y'

prop_divRndUp2 :: Integral a => a -> a -> Property
prop_divRndUp2 x y = y /= 0 ==> divRndUp x y == divRndUp2 x y

prop_clamp :: forall a. (Bounded a, Ord a) => a -> Property
prop_clamp x = (minBound :: a) <= (maxBound :: a)
               ==> minBound <= cx && cx <= maxBound
    where cx = clamp x

ceilFrac :: (Fractional a, RealFrac a, Integral b) => a -> a -> b
ceilFrac x y = ceiling $ x / y

divRndUp2 :: Integral a => a -> a -> a
divRndUp2 x y = let r | mod x y == 0 = 0
                      | otherwise   = 1
                in div x y + r
