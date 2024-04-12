{ self, ... }:
{
  perSystem = { simpleHaskellNix, self', ... }:
    let
      purus = simpleHaskellNix.mkPackage {
        name = "purus";
        src = ./.;

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = self.inputs.cardanoPackages;
        };
      };
    in
    {
      devShells.purus = purus.devShell;

      packages = {
        purs = purus.packages."purescript:exe:purs";
      };

      apps = {
        purs.program = "${self'.packages.purs}/bin/purs";
      };

      inherit (purus) checks;
    };
}
