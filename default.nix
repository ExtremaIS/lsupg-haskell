# Nix configuration for lsupg
#
# Usage:
#
# * Build lsupg with the default compiler:
#
#     $ nix-build
#
# * Build lsupg with a specific compiler version:
#
#     $ nix-build --argstr compiler ghc901

{ # This string argument specifies the compiler (example: "ghc8104").  When
  # not specified, the default compiler (configured below) is used.
  compiler ? null
  # This path argument specifies the packages to use.  When not specified, a
  # working revision for the selected compiler is used.  When a working
  # revision for the selected compiler is not defined (below), the packages
  # configured on the filesystem are used.
, nixpkgs ? null
  # This boolean argument is used by `shell.nix`.  When `True`, build tools
  # are added to the derivation.
, isShell ? false
}:

let

  # This string defines the default compiler version.
  defaultCompiler = "ghc8104";

  # This set defines working revisions for supported compiler versions.
  nixpkgsRevs = {
    # ghc901  = "4d4fdc329285e0d0c1c1a2b65947d651b8ba6b29"; NOTE text-short fails
    ghc8104 = "c92ca95afb5043bc6faa0d526460584eccff2277";
    ghc884  = "c92ca95afb5043bc6faa0d526460584eccff2277";
  };

  # This function fetches the specified nixpkgs revision.
  nixpkgsTarball = rev:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    };

  # This function fetches source from GitHub by tag.
  githubTagTarball = owner: repo: tag:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/refs/tags/${tag}.tar.gz";
    };

  # The compiler is explicitly specified or the default.
  compiler' = if isNull compiler then defaultCompiler else compiler;

  # Packages are explicitly specified, those for the revision defined for the
  # selected compiler, or those configured on the filesystem.
  pkgs = if isNull nixpkgs
    then if nixpkgsRevs ? ${compiler'}
      then import (nixpkgsTarball nixpkgsRevs.${compiler'}) {}
      else import <nixpkgs> {}
    else nixpkgs;

  # Git ignore functionality works with all supported revisions.
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

in

  # Configure the development environment for the package using the selected
  # packages and compiler.
  pkgs.haskell.packages.${compiler'}.developPackage {
    root = gitIgnore [./.gitignore] ./.;
    name = "lsupg";
    source-overrides = {
      ttc = githubTagTarball "ExtremaIS" "ttc-haskell" "ttc-haskell-1.1.0.1";
    };
    modifier = drv:
      if isShell
        then pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
          ])
        else drv;
  }
