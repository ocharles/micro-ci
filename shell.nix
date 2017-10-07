{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = (haskellPackages.override (import ./haskell-overrides.nix pkgs)).callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
