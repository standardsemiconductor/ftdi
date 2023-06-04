-- | Utility functions
module System.FTDI.Utils where

import Data.Bits

genFromEnum :: (Enum e, Num n) => e -> n
genFromEnum = fromIntegral . fromEnum

orBits :: (Num a, Bits a) => [a] -> a
orBits = foldr (.|.) 0

andBits :: (Num a, Bits a) => [a] -> a
andBits = foldr (.&.) $ complement 0

clamp :: (Bounded a, Ord a) => a -> a
clamp = atLeast minBound . atMost maxBound

atLeast :: Ord a => a -> a -> a
atLeast = max

atMost :: Ord a => a -> a -> a
atMost = min

divRndUp :: Integral a => a -> a -> a
divRndUp x y = let (d, m) = x `divMod` y
               in d + if m /= 0 then 1 else 0

between :: Ord a => a -> a -> a -> Bool
between lo hi x = (lo <= x) && (x <= hi)
