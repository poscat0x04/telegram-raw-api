{ mkDerivation, aeson, base, bytestring, deriving-aeson, fetchgit
, filepath, generic-lens, hpack, lens, mime-types, open-union
, servant, servant-multipart, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/poscat0x04/telegram-types";
    sha256 = "1k4clz54gbyqsmc2p949wk7x4h50pkzvlzfy3d0vlplzziv3ch94";
    rev = "942d6953a412e3c418890a98e9fb838e6485f135";
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
