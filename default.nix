{ mkDerivation, base, bytestring, hspec, QuickCheck, stdenv, zlib
}:
mkDerivation {
  pname = "zlib-bindings-awake";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base bytestring hspec QuickCheck zlib ];
  homepage = "http://github.com/themattchan/zlib-bindings";
  description = "Low-level bindings to the zlib package";
  license = stdenv.lib.licenses.bsd3;
}
