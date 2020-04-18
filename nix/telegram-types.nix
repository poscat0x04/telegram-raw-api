{ mkDerivation, aeson, base, bytestring, deriving-aeson, filepath
, generic-lens, lens, mime-types, open-union, servant
, servant-multipart, stdenv, text
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.2.0";
  sha256 = "94c4509a967ae4778c6a4121e699f2139da0b984ce552357d16b9f102a36d7ec";
  libraryHaskellDepends = [
    aeson base bytestring deriving-aeson filepath generic-lens lens
    mime-types open-union servant servant-multipart text
  ];
  testHaskellDepends = [
    aeson base bytestring deriving-aeson filepath generic-lens lens
    mime-types open-union servant servant-multipart text
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  doCheck = false;
  homepage = "https://github.com/poscat0x04/telegram-types#readme";
  description = "Types used in Telegram bot API";
  license = stdenv.lib.licenses.bsd3;
}
