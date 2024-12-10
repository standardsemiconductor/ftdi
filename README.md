# FTDI
[![Haskell CI](https://github.com/standardsemiconductor/ftdi/actions/workflows/haskell.yml/badge.svg)](https://github.com/standardsemiconductor/ftdi/actions/workflows/haskell.yml)
[![Hackage][hackage-badge]][hackage]

This library enables you to communicate with FTDI USB devices. It is implemented as a lightweight wrapper around the [usb](https://hackage.haskell.org/package/usb) library.

See [bindings-libusb](https://hackage.haskell.org/package/bindings-libusb) for instructions to install [libusb](https://libusb.info).

## Build Source (Ubuntu)

```bash
$ sudo apt install libusb-1.0-0-dev
$ git clone https://github.com/standardsemiconductor/ftdi.git
$ cd ftdi/
$ cabal build
$ cabal test
```

## Sample Usage

Find the first USB device matching the vendor ID and product ID:

```haskell
import qualified System.USB as USB
import qualified Data.Vector as V (toList)
import Data.List (find)
import System.FTDI

data Failure = FailureNotFound
             | FailureOther

findUSBDevice 
  :: USB.VendorId 
  -> USB.ProductId 
  -> IO (Either Failure (USB.Device, USB.Ctx))
findUSBDevice vendorId productId = do
  ctx <- USB.newCtx
  devDescs <- getDeviceDescs ctx
  return $ case fst <$> find (match . snd) devDescs of
    Nothing -> Left FailureNotFound
    Just dev -> Right (dev, ctx)
  where
    match :: USB.DeviceDesc -> Bool
    match devDesc =  USB.deviceVendorId  devDesc == vendorId
                  && USB.deviceProductId devDesc == productId

getDeviceDescs :: USB.Ctx -> IO [(USB.Device, USB.DeviceDesc)]
getDeviceDescs ctx = do
  devs <- V.toList <$> USB.getDevices ctx
  deviceDescs <- mapM USB.getDeviceDesc devs
  return $ zip devs deviceDescs
```

Setup an FT2232 FTDI device on interface A using MPSSE (Multi-Protocol Synchronous Serial Engine):

```haskell
withFTDI 
  :: USB.VendorId 
  -> USB.productId
  -> (InterfaceHandle -> IO (Either Failure a) 
  -> IO (Either Failure a)
withFTDI vendorId productId action = findUSBDevice vendorId productId >>= \case
  Left failure -> return $ Left failure
  Right (usbDevice, ctx) -> do
    ftdiDevice <- fromUSBDevice usbDevice ChipType_2232H
    withDeviceHandle ftdiDevice $ \devHndl -> do
      resetUSB devHndl
      withDetachedKernelDriver devHndl Interface_A $
        withInterfaceHandle devHndl Interface_A $ \ifHndl -> do
          reset ifHndl
          purgeReadBuffer ifHndl
          purgeWriteBuffer ifHndl
          setLatencyTimer ifHndl 1
          setBitMode ifHndl 0xFF BitMode_MPSSE
          action ifHndl
```

## References
* [FTDI Website](https://ftdichip.com/)
* [Application Note AN_108](https://www.ftdichip.com/Support/Documents/AppNotes/AN_108_Command_Processor_for_MPSSE_and_MCU_Host_Bus_Emulation_Modes.pdf): Command Processor for MPSSE and MCU Host Bus Emulation Modes
* [USB in a NutShell](https://www.beyondlogic.org/usbnutshell/usb1.shtml): Making sense of the USB standard

[hackage]:            <https://hackage.haskell.org/package/ftdi>
[hackage-badge]:      <https://img.shields.io/hackage/v/ftdi.svg?color=success>
