with (import <nixpkgs> {});
{
  micro-ci = (haskellPackages.override (import ./haskell-overrides.nix (import <nixpkgs> {}))).callPackage ./default.nix {};
}
