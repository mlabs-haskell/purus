with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    cabal-install
    haskell-language-server
    haskell.compiler.ghc924
    zlib
    libsodium
    secp256k1
  ];

  shelHook = ''
    export PKG_CONFIG_PATH=/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/local/lib/pkgconfig
    export PKG_CONFIG_PATH=/usr/share/pkgconfig:$PKG_CONFIG_PATH
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
    export LC_ALL=C.utf8
  '';
}
