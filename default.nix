# This file defines a function that takes a single optional argument 'pkgs'
# If pkgs is not set, it defaults to importing the nixpkgs found in NIX_PATH
{ pkgs ? import <nixpkgs> {} }:
let
   # Convenience alias for the standard environment
   stdenv = pkgs.stdenv;
in stdenv.mkDerivation {
  name = "psnix";
  # The source code is stored in our 'app' directory
  src = "./";
  # Our package depends on the nodejs package defined above
  buildInputs = with pkgs; [
    nodejs-7_x
    haskellPackages.purescript
  ];
  # This is useful for using this package with --run-env: the PORT environment variable
  PORT = "8888";
  # Our application has no ./configure script nor Makefile, installing simply involves
  # copying files from the source directory (set as cwd) to the designated output directory ($out).
  installPhase = ''
    mkdir -p $out
    cp -r * $out/
  '';
  shellHook = ''
    # Everything should be installed locally here, so put
    # exe's installed locally on the path.
    PATH=$PATH:./node_modules/.bin
  '';
}
