{ mkDerivation, async, base, base-unicode-symbols, bytestring
, stdenv, transformers, usb, vector
}:
mkDerivation {
  pname = "ftdi";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base base-unicode-symbols bytestring transformers usb vector
  ];
  description = "A thin layer over USB to communicate with FTDI chips";
  license = stdenv.lib.licenses.bsd3;
}
