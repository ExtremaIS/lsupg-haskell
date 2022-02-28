# Nix configuration for testing lsupg against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  lsupg-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  lsupg-ghc-8107 = import ./default.nix { compiler = "ghc8107"; };
  lsupg-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
