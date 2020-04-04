{ mkDerivation, aeson, base, bytestring, deriving-aeson, filepath
, hpack, mime-types, open-union, servant, servant-multipart, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/poscat0x04/telegram-types";
    sha256 = "1qwx71qn5kmaw52kfvwvx6xsd8799cwpv8bcg5sfhvx6cibvsxpc";
    rev = "e09187766d7c27a575fb881fe26fd6a9c9c3a67e";
    fetchSubmodules = true;
  };
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
