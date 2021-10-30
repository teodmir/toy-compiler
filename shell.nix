{ pkgs ? import <nixpkgs> {} }:

let e = (import ./. {}).env;
    devtools = with pkgs.haskellPackages; [
      happy alex
    ];
in e.overrideAttrs (oldAttrs: {
  nativeBuildInputs = oldAttrs.nativeBuildInputs ++ devtools;
})
