{ mkHaskellPackage
, fetchFromGitHub
}:

(mkHaskellPackage {
  name = "fourmolu";
  src = fetchFromGitHub {
    owner = "fourmolu";
    repo = "fourmolu";
    rev = "v0.13.1.0";
    hash = "sha256-abUK9KdvVI7di84X/L3vHZM97pOsciyx503aDjUnoc4=";
  };
}).packages."fourmolu:exe:fourmolu"
