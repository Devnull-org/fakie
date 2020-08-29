let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./fakie.nix { }
