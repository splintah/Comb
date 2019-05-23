{ mkDerivation, base, containers, HUnit, stdenv, text }:
mkDerivation {
  pname = "Comb";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers text ];
  testHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/splintah/Comb";
  description = "Parser combinator library";
  license = stdenv.lib.licenses.agpl3;
}
