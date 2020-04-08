{ mkDerivation, aeson, base, bytestring, deriving-aeson, fetchgit
, filepath, generic-lens, hpack, lens, mime-types, open-union
, servant, servant-multipart, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/poscat0x04/telegram-types";
    sha256 = "1zgl20nh1k070zq7h3rwmb8rlmmbsipsqpm5xqd27wfa6qh38ic4";
    rev = "5b79310e6b923eb43947c786776dcce24b78c296";
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
