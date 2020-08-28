{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", withHoogle ? true }:
let
  inherit (nixpkgs) pkgs;
  pinnedUnstable =
    pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      rev = "c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38";
      sha256 = "1ak7jqx94fjhc68xh1lh35kh3w3ndbadprrb762qgvcfb8351x8v";
    };
  unstable = import pinnedUnstable {};
  origBuild = (import ./default.nix) {};

  finalPackage = pkgs.haskell.lib.overrideCabal origBuild (drv: {
    libraryToolDepends = drv.libraryToolDepends ++ [
      pkgs.stack
      pkgs.hlint
      pkgs.stdenv
    ];
    librarySystemDepends = [ pkgs.zlib ];
    license = pkgs.stdenv.lib.licenses.bsd3;
    shellHook = ''
      '';
  });

  haskellGhc =
    if compiler == "default"
      then pkgs.haskellGhc
      else unstable.haskell.packages.${compiler};

  hspkgs =
    if withHoogle
       then
         haskellGhc.override {
           overrides = (self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           });
         }
       else haskellGhc;

  drv = hspkgs.callPackage finalPackage {};
in
  if pkgs.lib.inNixShell then drv.env else drv
