{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    idris2
    idris2Packages.idris2Lsp
    rlwrap
  ];

  shellHook = ''
    echo "Idris2 development environment"
    echo "Idris2 version: $(idris2 --version)"
  '';
}
