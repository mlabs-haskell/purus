{ self, ... }:
{
  perSystem = { self', config, ... }:
    let
      purus = config.libHaskell.mkPackage {
        name = "purus";
        src = ./.;

        externalRepositories = {
          "https://chap.intersectmbo.org/" = self.inputs.CHaP;
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
    };
}
