{ mkDerivation, aeson, base, bytestring, deriving-aeson, filepath
, generic-lens, lens, mime-types, open-union, servant
, servant-multipart, stdenv, text, time
}:
mkDerivation {
  pname = "telegram-types";
  version = "0.4.0";
  sha256 = "9552e1b31508a09b321eb5857ddc29e9995d91e6422db63562cde5114e0b3bbe";
  libraryHaskellDepends = [
    aeson base bytestring deriving-aeson filepath generic-lens lens
    mime-types open-union servant servant-multipart text time
  ];
  testHaskellDepends = [
    aeson base bytestring deriving-aeson filepath generic-lens lens
    mime-types open-union servant servant-multipart text time
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  doCheck = false;
  homepage = "https://github.com/poscat0x04/telegram-types#readme";
  description = "Types used in Telegram bot API";
  license = stdenv.lib.licenses.bsd3;
}
