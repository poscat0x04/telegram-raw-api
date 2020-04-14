{ mkDerivation, aeson, base, bytestring, deriving-aeson, fetchgit
, filepath, generic-lens, hpack, lens, mime-types, open-union
, servant, servant-multipart, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/poscat0x04/telegram-types";
    sha256 = "0ipzsdgs4m8wn1ifajycxn98zb3d8kfz1axhw5gvzjvb7ijrblsc";
    rev = "26aa9434bd96068aded732a99e985dad4c2ba649";
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
