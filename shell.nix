with (import <nixpkgs> {});
let haskell928 = haskell.packages.ghc928;
    ghc928 = haskell.compiler.ghc928;
in mkShell {
  nativeBuildInputs = [
    pkg-config
    haskell928.haskell-language-server
    ghc928
    cabal-install
  ];

  buildInputs = [
    zlib
    libsodium
    secp256k1
  ];

  shellHook = ''
    export LC_ALL=C.utf8
  '';
}
