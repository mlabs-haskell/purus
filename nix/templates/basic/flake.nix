{
  description = "Example flake to develop with Purus";

  inputs = {
    purus.url = "github:mlabs-haskell/purus";
    nixpkgs.follows = "purus/nixpkgs";
  };

  outputs = { nixpkgs, purus, ... }:
    let
      systems = [ "x86_64-linux" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = eachSystem (system: {
        default = nixpkgs.legacyPackages.${system}.mkShell {
          packages = [
            purus.packages.${system}.purs
            purus.packages.${system}.purus
          ];
        };
      });
    };
}
