{ mkDerivation, base, lib, opt-env-conf, opt-env-conf-test, sydtest
, sydtest-discover, text, time, unliftio
}:
mkDerivation {
  pname = "looper";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base opt-env-conf text time unliftio ];
  testHaskellDepends = [
    base opt-env-conf opt-env-conf-test sydtest unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/looper#readme";
  license = lib.licenses.mit;
}
