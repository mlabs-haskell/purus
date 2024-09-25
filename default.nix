{
  perSystem = { self', pkgs, config, ... }:
    let
      cardanoPackages = pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-haskell-packages";
        rev = "149e5f9c0519927a79c0e3132af3ff22b795ac0c"; # branch: repo
        hash = "";
      };

      purus = config.libHaskell.mkPackage {
        name = "purus";
        src = ./.;

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
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
