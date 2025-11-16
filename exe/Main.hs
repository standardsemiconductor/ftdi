import Control.Monad
import Data.Foldable
import Options.Applicative
import System.FTDI
import qualified System.USB as USB

data Cli
  = SetBaudRate USB.VendorId USB.ProductId ChipType Interface Integer

main :: IO ()
main = ftdi =<< runCli

runCli :: IO Cli
runCli = customExecParser prefs' $ info (cliParser <**> helper) mempty
  where
    prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

cliParser :: Parser Cli
cliParser =
  SetBaudRate
    <$> vendorIdOptParser
    <*> productIdOptParser
    <*> chipTypeOptParser
    <*> interfaceOptParser
    <*> baudOptParser

vendorIdOptParser :: Parser USB.VendorId
vendorIdOptParser = option auto $ long "vid" <> help "Vendor ID"

productIdOptParser :: Parser USB.ProductId
productIdOptParser = option auto $ long "pid" <> help "Product ID"

chipTypeOptParser :: Parser ChipType
chipTypeOptParser = asum
  [ flag' ChipType_AM    $ long "AM"
  , flag' ChipType_BM    $ long "BM"
  , flag' ChipType_2232C $ long "2232C"
  , flag' ChipType_R     $ long "R"
  , flag' ChipType_2232H $ long "2232H"
  , flag' ChipType_4232H $ long "4232H"
  ]

interfaceOptParser :: Parser Interface
interfaceOptParser = asum
  [ flag' Interface_A $ short 'A'
  , flag' Interface_B $ short 'B'
  , flag' Interface_C $ short 'C'
  , flag' Interface_D $ short 'D'
  ]

baudOptParser :: Parser Integer
baudOptParser = option auto $ long "baud" <> help "Baud rate"

ftdi :: Cli -> IO ()
ftdi (SetBaudRate vid pid chipType iface baud) = do
  devices <- USB.getDevices =<< USB.newCtx
  selects <- filterM (selectDevice vid pid) $ toList devices
  case selects of
    []    -> putStrLn "No device found"
    [dev] -> setBaud iface baud =<< fromUSBDevice dev chipType
    _     -> putStrLn "More than one device found"

selectDevice :: USB.VendorId -> USB.ProductId -> USB.Device -> IO Bool
selectDevice vid pid dev = do
  desc <- USB.getDeviceDesc dev
  return $ USB.deviceVendorId desc == vid
           && USB.deviceProductId desc == pid

setBaud :: Interface -> Integer -> Device -> IO ()
setBaud iface baud dev =
  withDeviceHandle dev $ \dHndl ->
  withDetachedKernelDriver dHndl iface $
  withInterfaceHandle dHndl iface $ \iHndl -> do
    putStrLn $ "setting baud rate: " <> show baud
    baud' <- setBaudRate iHndl $ fromIntegral baud :: IO (BaudRate Double)
    putStrLn $ "set baud rate: " <> show baud'
