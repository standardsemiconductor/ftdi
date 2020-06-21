{ mkDerivation, async, base, base-unicode-symbols, bytestring
, stdenv, transformers, vector, pkg-config, libusb1
}:
mkDerivation {
  pname = "ftdi";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base base-unicode-symbols bytestring transformers vector
  ];
  librarySystemDepends = [ libusb1 pkg-config ];
  description = "A thin layer over USB to communicate with FTDI chips";
  license = stdenv.lib.licenses.bsd3;
}
