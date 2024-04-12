{
  description = "uplc-benchmark";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    hci-effects = {
      url = "github:hercules-ci/hercules-ci-effects";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    simpleHaskellNix = {
      url = "github:mlabs-haskell/simple-haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks-nix.follows = "pre-commit-hooks-nix";
      inputs.hci-effects.follows = "hci-effects";
      inputs.flake-parts.follows = "flake-parts";
    };
    cardanoPackages = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
    imports = [
      inputs.pre-commit-hooks-nix.flakeModule
      inputs.hci-effects.flakeModule
      inputs.simpleHaskellNix.flakeModules.simpleHaskellNix
      ./.
    ];

    # `nix flake show --impure` hack
    systems =
      if builtins.hasAttr "currentSystem" builtins
      then [ builtins.currentSystem ]
      else inputs.nixpkgs.lib.systems.flakeExposed;

    herculesCI.ciSystems = [ "x86_64-linux" ];

    perSystem =
      { config
      , pkgs
      , system
      , self'
      , ...
      }: {
        _module.args.pkgs = import self.inputs.nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        pre-commit.settings = {
          hooks = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            # TODO: Enable in separate PR, causes mass changes.
            # fourmolu.enable = true;
          };
        };

        devShells = {
          default = pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;

            inputsFrom = [
              self'.devShells.purus
            ];
          };
        };
      };
  });
}
