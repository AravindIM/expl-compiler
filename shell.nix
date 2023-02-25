{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "expl-shell";
  packages = [  ];
  shellHook = "rustup default stable";
  buildInputs = with pkgs; [ 
    rust-analyzer
    rustup
  ];
}

