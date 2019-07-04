let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {};

in

with pkgs;

let
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      llvm-hs = super.llvm-hs.override {
        llvm-config = pkgs.llvm_8;
      };
      llvm-hs-pure = super.llvm-hs-pure_8_0_0;
      accelerate-llvm-native = haskell.lib.dontCheck super.accelerate-llvm-native;
    };
  };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    accelerate
    accelerate-llvm
    accelerate-llvm-native
    errors
  ]);
in

stdenv.mkDerivation {
  name = "accelodiff-dev";
  buildInputs = [
    hsenv
  ];
  shellHook = ''
  '';
}