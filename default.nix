{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall filepath hakyll highlightingKate pandoc time;

in cabal.mkDerivation (self: {
  pname = "noneio";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ filepath hakyll highlightingKate pandoc time ];
  buildTools = [ cabalInstall ];
})
