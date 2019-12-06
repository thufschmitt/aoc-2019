{ pkgs ? import <nixpkgs> {} }:
pkgs.ocamlPackages.buildDunePackage rec {
  pname = "aoc";
  version = "2019";

  propagatedBuildInputs = [ pkgs.ocamlPackages.containers pkgs.ocamlPackages.gen pkgs.ocamlPackages.sequence ];
  buildInputs = [ pkgs.ocamlPackages.merlin ];

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
}
