{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, directory, filepath, github
      , process, servant-github-webhook, servant-server, stdenv, text
      , warp, dhall
      }:
      mkDerivation {
        pname = "micro-ci";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base directory filepath github process servant-github-webhook
          servant-server text warp dhall
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = (haskellPackages.override {
    overrides = self: super: {
      github = self.callPackage ../github {};
      servant-github-webhook = pkgs.haskell.lib.doJailbreak super.servant-github-webhook;
    };
  }).callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
