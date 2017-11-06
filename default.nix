{ mkDerivation, base, bytestring, hspec, QuickCheck, stdenv, zlib
}:
mkDerivation {
  pname = "zlib-bindings";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base bytestring hspec QuickCheck zlib ];
  homepage = "http://github.com/snoyberg/zlib-bindings";
  description = "Low-level bindings to the zlib package. (deprecated)";
  license = stdenv.lib.licenses.bsd3;
}
