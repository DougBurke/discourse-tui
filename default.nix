{ compiler ? "ghc92"
}:
(import ./release.nix { compiler = compiler; }).exe
