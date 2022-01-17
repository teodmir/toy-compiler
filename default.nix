{ pkgs ? import <nixpkgs> {} }:

let instr = pkgs.haskellPackages.callCabal2nix "test-compiler" ./. {};
in with pkgs.haskellPackages; instr.overrideAttrs (oldAttrs: rec {
  nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [alex happy];
})
