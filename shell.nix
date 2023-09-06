{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [ llvmPackages_15.llvm libffi libxml2 ];
}
