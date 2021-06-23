# Nix configuration for testing lsupg against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  lsupg-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  lsupg-ghc-8104 = import ./default.nix { compiler = "ghc8104"; };
  # lsupg-ghc-901 = import ./default.nix { compiler = "ghc901"; }; NOTE text-short fails
}
