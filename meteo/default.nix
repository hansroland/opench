{ mkDerivation, aeson, base, data-default, stdenv, text, time }:
mkDerivation {
  pname = "opench-meteo";
  version = "0.2.0.0";
  sha256 = "1h3slv334cx571l1k113qq0fg9ggznp0f0cabrdm7lr1jldn94wy";
  libraryHaskellDepends = [ aeson base data-default text time ];
  homepage = "https://github.com/hansroland/opench";
  description = "A Haskell implementation of the Swiss Meteo Net data API";
  license = stdenv.lib.licenses.bsd3;
}
