{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      llvm-hs = super.llvm-hs.override {
        llvm-config = pkgs.llvm_8;
      };
      llvm-hs-pure = super.llvm-hs-pure_8_0_0;
    };
  };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    accelerate-llvm
    accelerate
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