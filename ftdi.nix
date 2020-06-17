{ mkDerivation, async, base, base-unicode-symbols, bytestring
, derive, QuickCheck, random, stdenv, tagged, test-framework
, test-framework-quickcheck2, transformers, usb, vector
}:
mkDerivation {
  pname = "ftdi";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base base-unicode-symbols bytestring transformers usb vector
  ];
  testHaskellDepends = [
    derive QuickCheck random tagged test-framework
    test-framework-quickcheck2
  ];
  description = "A thin layer over USB to communicate with FTDI chips";
  license = stdenv.lib.licenses.bsd3;
}
