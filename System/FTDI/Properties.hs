{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module System.FTDI.Properties where

import Control.Arrow
import Data.Bits
import Data.Word
import Generic.Random        ( genericArbitrary, uniform )
import System.FTDI           ( ModemStatus(..), ChipType(..)
                             , BaudRate(..), nearestBaudRate
                             )
import System.FTDI.Internal  ( marshalModemStatus
                             , unmarshalModemStatus
                             )
import Test.QuickCheck       ( Arbitrary, arbitrary, shrink, choose
                             , frequency
                             )
import System.Random         ( Random )

----------------
-- Properties --
----------------

prop_marshalModemStatus :: ModemStatus -> Bool
prop_marshalModemStatus =
    isIdentity ( uncurry unmarshalModemStatus
               . marshalModemStatus
               )

prop_unmarshalModemStatus :: (Word8, Word8) -> Bool
prop_unmarshalModemStatus =
    -- The identity only holds when we ignore the 4 least significant bytes.
    isIdentityWith (\x -> (ignoreBits x ==))
                   ( marshalModemStatus
                   . uncurry unmarshalModemStatus
                   . ignoreBits
                   )
    where ignoreBits = first (.&. 0xf0)

prop_baudRateError :: RealFrac α => α -> (ChipType -> BaudRate α -> Bool)
prop_baudRateError maxError chip baudRate =
    let b = nearestBaudRate chip baudRate
        e = abs (b - baudRate) / baudRate
    in unBaudRate e <= maxError


-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

isIdentity :: Eq α => (α -> α) -> (α -> Bool)
isIdentity = isIdentityWith (==)

isIdentityWith :: Eq α => (α -> α -> Bool) -> (α -> α) -> (α -> Bool)
isIdentityWith eq = liftA2 eq id


-------------------------------------------------------------------------------
-- Arbitrary instances
-------------------------------------------------------------------------------

deriving instance Random α => Random (BaudRate α)

instance (Random α, Num α, Arbitrary α) => Arbitrary (BaudRate α) where
    arbitrary = frequency [ ( 1500000 - unBaudRate (minBound :: BaudRate Int)
                            , choose (minBound, 1500000)
                            )
                          , (1, return 2000000)
                          , (1, return 3000000)
                          ]
    shrink = map BaudRate . shrink . unBaudRate

instance Arbitrary ModemStatus where
  arbitrary = genericArbitrary uniform

instance Arbitrary ChipType where
  arbitrary = genericArbitrary uniform

