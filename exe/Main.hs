import Data.Foldable
import Data.Word
import Numeric
import Options.Applicative
import System.FTDI
import qualified System.USB as USB

main :: IO ()
main = ftdi =<< runCli

data Cli
  = SetBaudCmd SetBaud
  | ListUsbCmd

data SetBaud
  = SetBaud
      USB.VendorId
      USB.ProductId
      (Maybe ChipType)
      Interface
      Integer -- baud rate

runCli :: IO Cli
runCli = customExecParser prefs' $ info (cliParser <**> helper) mempty
  where
    prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

cliParser :: Parser Cli
cliParser = hsubparser $ mconcat
  [ command "set-baud" $ info (SetBaudCmd <$> setBaudParser) mempty
  , command "list-usb" $
      info (pure ListUsbCmd) $
        progDesc "List USB device descriptors"
  ]

setBaudParser :: Parser SetBaud
setBaudParser =
  SetBaud
    <$> vendorIdParser
    <*> productIdParser
    <*> optional chipTypeParser
    <*> interfaceParser
    <*> baudParser

vendorIdParser :: Parser USB.VendorId
vendorIdParser = option auto $ long "vid" <> help "Vendor ID"

productIdParser :: Parser USB.ProductId
productIdParser = option auto $ long "pid" <> help "Product ID"

chipTypeParser :: Parser ChipType
chipTypeParser = asum
  [ flag' ChipType_AM    $ long "AM"
  , flag' ChipType_BM    $ long "BM"
  , flag' ChipType_2232C $ long "2232C"
  , flag' ChipType_R     $ long "R"
  , flag' ChipType_2232H $ long "2232H"
  , flag' ChipType_4232H $ long "4232H"
  ]

interfaceParser :: Parser Interface
interfaceParser = asum
  [ flag' Interface_A $ short 'A'
  , flag' Interface_B $ short 'B'
  , flag' Interface_C $ short 'C'
  , flag' Interface_D $ short 'D'
  ]

baudParser :: Parser Integer
baudParser = option auto $ long "baud" <> help "Baud rate"

ftdi :: Cli -> IO ()
ftdi cli = case cli of
  SetBaudCmd (SetBaud vid pid chipTypeM iface baud) ->
    setBaud iface baud =<< findFtdiDevice chipTypeM (match vid pid)
  ListUsbCmd ->
    putStrLn . unlines . map (renderUsbDeviceDesc . snd) =<< getDeviceDescs

findFtdiDevice :: Maybe ChipType -> (USB.DeviceDesc -> Bool) -> IO Device
findFtdiDevice chipTypeM p = do
  deviceDescs <- getDeviceDescs
  case find (p . snd) deviceDescs of
    Nothing   -> fail "no ftdi device found"
    Just  d   -> case findChipType chipTypeM (snd d) of
      Nothing -> fail "unknown ftdi chip"
      Just ct -> fromUSBDevice (fst d) ct

findChipType :: Maybe ChipType -> USB.DeviceDesc -> Maybe ChipType
findChipType Nothing d = guessChipType d
findChipType ct      _ = ct

getDeviceDescs :: IO [(USB.Device, USB.DeviceDesc)]
getDeviceDescs = do
  devices     <- fmap toList $ USB.getDevices =<< USB.newCtx
  deviceDescs <- mapM USB.getDeviceDesc devices
  return $ zip devices deviceDescs

match :: USB.VendorId -> USB.ProductId -> USB.DeviceDesc -> Bool
match vid pid desc =
  USB.deviceVendorId  desc == vid &&
  USB.deviceProductId desc == pid

setBaud :: Interface -> Integer -> Device -> IO ()
setBaud iface baud dev =
  withDeviceHandle dev $ \dHndl ->
  withDetachedKernelDriver dHndl iface $
  withInterfaceHandle dHndl iface $ \iHndl -> do
    putStrLn $ "setting baud rate: " <> show baud
    baud' <- setBaudRate iHndl $ fromIntegral baud :: IO (BaudRate Double)
    putStrLn $ "set baud rate: " <> show baud'

renderUsbDeviceDesc :: USB.DeviceDesc -> String
renderUsbDeviceDesc d =
  unlines $ "USB Device Descriptor" : map (" " <>)
    [ "Specification  : " <> show (USB.deviceUSBSpecReleaseNumber d)
    , "Class          : " <> show (USB.deviceClass d)
    , "SubClass       : " <> show (USB.deviceSubClass d)
    , "Protocol       : " <> show (USB.deviceProtocol d)
    , "Max packet size: " <> show (USB.deviceMaxPacketSize0 d)
    , "Vendor ID      : " <> hex16 (USB.deviceVendorId d)
    , "Product ID     : " <> hex16 (USB.deviceProductId d)
    , "Release        : " <> show (USB.deviceReleaseNumber d)
    , "Manufacture    : " <> show (USB.deviceManufacturerStrIx d)
    , "Product        : " <> show (USB.deviceProductStrIx d)
    , "Serial         : " <> show (USB.deviceSerialNumberStrIx d)
    , "Configs        : " <> show (USB.deviceNumConfigs d)
    ]

hex16 :: Word16 -> String
hex16 w16 = "0x" <> showHex w16 ""
