{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Access discourse via a Haskell TUI and a devShell. Based on https://builds.sr.ht/~jackwines/discourse-tui";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      # supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        discourse-tui = final.haskellPackages.callCabal2nix "discourse-tui" ./. {};
      });
      packages = forAllSystems (system: {
         discourse-tui = nixpkgsFor.${system}.discourse-tui;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.discourse-tui);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.discourse-tui];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
