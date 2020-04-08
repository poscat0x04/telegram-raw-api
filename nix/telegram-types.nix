{ mkDerivation, aeson, base, bytestring, deriving-aeson, fetchgit
, filepath, generic-lens, hpack, lens, mime-types, open-union
, servant, servant-multipart, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/poscat0x04/telegram-types";
    sha256 = "1vrzxy5fm20y0bnh3amw4qshbrq76k992l3h89kdgvbsvy37myy3";
    rev = "dcaee11ccf6dbd7c15333596bde538bc6f27ebd9";
    fetchSubmodules = false;
  };
  libraryHaskellDepends = [
    aeson base bytestring deriving-aeson filepath generic-lens lens
    mime-types open-union servant servant-multipart text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring deriving-aeson filepath generic-lens lens
    mime-types open-union servant servant-multipart text
    unordered-containers
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/poscat0x04/telegram-types#readme";
  license = stdenv.lib.licenses.bsd3;
}
