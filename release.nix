let
  #nixpkgs = fetchGit {
  #  url = git://github.com/NixOS/nixpkgs-channels;
  #  # ref = "nixos-19.03";
  #  ref = "nixos-20.03";
  #};

  nixpkgs = import <nixpkgs> {};

  # How do I specify "the latest" ghc?
  # compiler = "ghc";
  compiler = "ghc8103";

  inherit (nixpkgs) pkgs;

  # since we are in a sub-directory
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "discourse-tui" =
        hself.callCabal2nix "discourse-tui" (gitignore ./.) {};

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
