{ mkDerivation, aeson, base, bytestring, deriving-aeson, filepath
, hpack, mime-types, open-union, servant, servant-multipart, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = ../../telegram-types;
  libraryHaskellDepends = [
    aeson base bytestring deriving-aeson filepath mime-types open-union
    servant servant-multipart text unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring deriving-aeson filepath mime-types open-union
    servant servant-multipart text unordered-containers
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/poscat0x04/telegram-types#readme";
  license = stdenv.lib.licenses.bsd3;
}
