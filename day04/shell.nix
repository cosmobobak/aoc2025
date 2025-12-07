{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    koka
  ];

  shellHook = ''
    echo "Koka $(koka --version) ready"
  '';
}
