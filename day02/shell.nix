{ pkgs ? import <nixpkgs> {} }:
let
  hp = pkgs.haskell.packages.ghc96;
in
pkgs.mkShell {
  buildInputs = [
    (hp.ghcWithPackages (ps: with ps; [
      containers text vector split megaparsec list-duplicate
    ]))
    hp.haskell-language-server
  ];
}
