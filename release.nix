{ sources ? import ./nix/sources.nix
# , nixpkgs ? import sources.nixpkgs {}
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc8107"
}:

let
  # inherit (nixpkgs) pkgs;
  
  # since we are in a sub-directory
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "discourse-tui" =
        hself.callCabal2nix "discourse-tui" (gitignore ./.) {};

      # formatting =
      #   hself.callHackage "formatting" "7.1.2" {};
      # formatting = hself.formatting_7_1_2;
      
      #validity =
      #  hself.callHackage "validity" "0.8.0.0" {};

      #cursor =
      #  hself.callHackage "cursor" "0.0.0.1" {};

    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."discourse-tui"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.niv
    ];
    # withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."discourse-tui");

in
  {
   inherit shell;
   inherit exe;
   inherit myHaskellPackages;
   "discourse-tui" = myHaskellPackages."discourse-tui";
  }
