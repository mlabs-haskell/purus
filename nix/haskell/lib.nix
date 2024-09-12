{ lib
, fetchFromGitHub
  # e.g. "x86_64-linux"
, system # : string
  # , haskellNixNixpkgs # : nixpkgs
  # , haskellNixOverlay # : overlay
, inputs
}:

let
  iohk-nix = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-nix";
    rev = "4848df60660e21fbb3fe157d996a8bac0a9cf2d6";
    hash = "sha256-ediFkDOBP7yVquw1XtHiYfuXKoEnvKGjTIAk9mC6qxo=";
  };

  pkgs =
    import inputs.haskell-nix.inputs.nixpkgs {
      inherit system;
      overlays = [
        inputs.haskell-nix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.iohk-nix.overlays.haskell-nix-crypto
      ];
      inherit (inputs.haskell-nix) config;
    };
in

{ name # : string
, src # : path
, ghcVersion ? "ghc928" # : string
, haskellModules ? [ ]
, externalDependencies ? [ ]
  # , externalRepositories ? { }
}:
let
  mkHackage = pkgs.callPackage ./mk-hackage.nix {
    nix-tools = pkgs.haskell-nix.nix-tools-set {
      compiler-nix-name = ghcVersion;
    };
  };

  # This looks like a noop but without it haskell.nix throws a runtime
  # error about `pkgs` attribute not being present which is nonsense
  # https://input-output-hk.github.io/haskell.nix/reference/library.html?highlight=cabalProject#modules
  fixedHaskellModules = map (m: args @ { ... }: m args) haskellModules;

  flatExternalDependencies =
    lib.lists.concatMap
      (dep: [ (dep.passthru or { }).src or dep ] ++
        (flatExternalDependencies (dep.passthru or { }).externalDependencies or [ ]));

  flattenedExternalDependencies = flatExternalDependencies externalDependencies;

  customHackages = mkHackage {
    srcs = map toString flattenedExternalDependencies;
    inherit name;
  };

  project = pkgs.haskell-nix.cabalProject' {
    inherit src;
    name = name;

    compiler-nix-name = ghcVersion;
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
    };

    modules = customHackages.modules ++ fixedHaskellModules;
    inherit (customHackages) extra-hackages extra-hackage-tarballs;

    index-state = "2024-06-17T12:18:52Z";

    shell = {
      withHoogle = true;
      exactDeps = true;

      tools = {
        cabal = { };
        haskell-language-server = { };
      };
    };
  };

  projectFlake = project.flake { };

  augmentedPackages = builtins.mapAttrs
    (_: package:
      package // {
        passthru = (package.passthru or { }) // {
          inherit src externalDependencies;
        };
      })
    (projectFlake.packages or { });
in
projectFlake // {
  packages = augmentedPackages;
}
