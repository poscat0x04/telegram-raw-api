{ mkDerivation, aeson, base, bytestring, deriving-aeson, fetchgit
, filepath, generic-lens, hpack, lens, mime-types, open-union
, servant, servant-multipart, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/poscat0x04/telegram-types";
    sha256 = "01ixk1lbdfjds6p6kjsw9ra7nskr1qizn53g2hdw2bvww4idvy0b";
    rev = "172c1da91d28e474e18fc74065b9c64a136bc9b6";
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
