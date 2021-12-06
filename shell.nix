{ pkgs, name }:

pkgs.stable.mkShell {
  inherit name;

  nativeBuildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Native Libraries:
    { }

    ###################################################
    # Languages:
    {
      inherit (pkgs.stable) dhall nodejs-16_x;
      inherit (pkgs.unstable) purescript;
      inherit (pkgs.stable.nodePackages) typescript;
    }

    ###################################################
    # Code styles:
    {
      inherit (pkgs.stable)
        pre-commit
        purs-tidy
        nixpkgs-fmt
        nix-linter
        shfmt
        shellcheck;
      inherit (pkgs.stable.python3Packages) pre-commit-hooks yamllint;
      inherit (pkgs.stable.nodePackages) prettier;

      headroom = pkgs.stable.haskell.lib.justStaticExecutables pkgs.stable.haskellPackages.headroom;
    }

    ###################################################
    # Command line tools:
    { inherit (pkgs.stable) gitFull git-lfs; }

    ###################################################
    # Language servers:
    {
      inherit (pkgs.stable.nodePackages)
        bash-language-server
        purescript-language-server
        typescript-language-server
        vscode-html-languageserver-bin
        vscode-json-languageserver-bin
        yaml-language-server;
    }

    ###################################################
    # Package managers:
    { inherit (pkgs.stable) spago; }
  ];
}
