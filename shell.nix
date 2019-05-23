{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, HUnit, stdenv, text }:
      mkDerivation {
        pname = "Comb";
        version = "0.2.0.0";
        src = ./.;
        libraryHaskellDepends = [ base containers text ];
        testHaskellDepends = [ base HUnit ];
        homepage = "https://github.com/splintah/Comb";
        description = "Parser combinator library";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
