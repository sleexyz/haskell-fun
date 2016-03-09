{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, base, containers, pretty-show
      , stdenv, unicode-prelude
      }:
      mkDerivation {
        pname = "haskell-fun";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          ansi-terminal base containers pretty-show unicode-prelude
        ];
        testHaskellDepends = [ base ];
        homepage = "http://github.com/sleexyz/haskell-fun#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
