{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, lib
, opt-env-conf, opt-env-conf-test, sydtest, sydtest-discover, text
, time, unliftio
}:
mkDerivation {
  pname = "looper";
  version = "0.2.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base opt-env-conf text time unliftio
  ];
  testHaskellDepends = [
    autodocodec-yaml base opt-env-conf opt-env-conf-test sydtest
    unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/looper#readme";
  license = lib.licenses.mit;
}
