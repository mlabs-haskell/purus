{ lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, pkgs, ... }: {
      options = {
        libUtils = mkOption {
          type = types.anything;
          default = { };
        };
      };

      config.libUtils = pkgs.callPackage ./lib.nix { };
    });
  };
}
