{ self
, lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, system, pkgs, ... }: {
      options = {
        libHaskell = mkOption {
          type = types.anything;
          default = { };
        };
      };

      config =
        let
          mkHaskellPackage = pkgs.callPackage ./lib.nix {
            inherit lib system;
            haskellNixNixpkgs = self.inputs.haskell-nix.inputs.nixpkgs;
            haskellNixOverlay = self.inputs.haskell-nix.overlay;
          };

        in
        {
          libHaskell = {
            mkPackage = mkHaskellPackage;
          };
        };
    });
  };
}
