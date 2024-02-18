{
  perSystem = { self', config, ... }:
    let
      purus = config.libPlutarch.mkPackage {
        name = "purus";
        src = ./.;
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
