{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, envparse, lib, optparse-applicative, sydtest, sydtest-discover
, text, time, timeout, unliftio, weigh
}:
mkDerivation {
  pname = "looper";
  version = "0.2.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base envparse optparse-applicative text time
    unliftio
  ];
  executableHaskellDepends = [ base timeout weigh ];
  testHaskellDepends = [
    autodocodec-yaml base optparse-applicative sydtest unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/looper#readme";
  license = lib.licenses.mit;
  mainProgram = "looper-weigh";
}
