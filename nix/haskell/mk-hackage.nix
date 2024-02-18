{ gzip
, runCommand
, lib
, nix-tools
}:
let
  mkPackageSpec = src:
    with lib;
    let
      cabalFiles = concatLists (mapAttrsToList
        (name: type: if type == "regular" && hasSuffix ".cabal" name then [ name ] else [ ])
        (builtins.readDir src));

      cabalPath =
        if length cabalFiles == 1
        then src + "/${builtins.head cabalFiles}"
        else builtins.abort "Could not find unique file with .cabal suffix in source: ${src}";
      cabalFile = builtins.readFile cabalPath;
      parse = field:
        let
          lines = filter (s: builtins.match "^${field} *:.*$" (toLower s) != null) (splitString "\n" cabalFile);
          line =
            if lines != [ ]
            then head lines
            else builtins.abort "Could not find line with prefix ''${field}:' in ${cabalPath}";
        in
        replaceStrings [ " " ] [ "" ] (head (tail (splitString ":" line)));
      pname = parse "name";
      version = parse "version";
    in
    { inherit src pname version; };

  mkHackageDir = { pname, version, src }:
    runCommand "${pname}-${version}-hackage"
      { } ''
      set -e
      mkdir -p $out/${pname}/${version}
      md5=11111111111111111111111111111111
      sha256=1111111111111111111111111111111111111111111111111111111111111111
      length=1
      cat <<EOF > $out/"${pname}"/"${version}"/package.json
      {
        "signatures" : [],
        "signed" : {
            "_type" : "Targets",
            "expires" : null,
            "targets" : {
              "<repo>/package/${pname}-${version}.tar.gz" : {
                  "hashes" : {
                    "md5" : "$md5",
                    "sha256" : "$sha256"
                  },
                  "length" : $length
              }
            },
            "version" : 0
        }
      }
      EOF
      cp ${src}/*.cabal $out/"${pname}"/"${version}"/
    '';

  mkHackageTarballFromDirs = name: hackageDirs:
    runCommand "${name}-hackage-index.tar.gz" { } ''
      mkdir hackage
      ${builtins.concatStringsSep "" (map (dir: ''
        echo ${dir}
        ln -sf ${dir}/* hackage/
      '') hackageDirs)}
      cd hackage
      tar --sort=name --owner=root:0 --group=root:0 --mtime='UTC 2009-01-01' -hczvf $out */*/*
    '';

  mkHackageTarball = name: pkg-specs:
    mkHackageTarballFromDirs name (map mkHackageDir pkg-specs);

  mkHackageNix = name: hackageTarball:
    runCommand "${name}-hackage-nix"
      {
        nativeBuildInputs = [
          gzip
          nix-tools
        ];
      } ''
      set -e
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      cp ${hackageTarball} 01-index.tar.gz
      gunzip 01-index.tar.gz
      hackage-to-nix $out 01-index.tar "https://mkHackageNix/"
    '';

  mkModule = extraHackagePackages: {
    packages = lib.listToAttrs (map
      (spec: {
        name = spec.pname;
        value = {
          inherit (spec) src;
        };
      })
      extraHackagePackages);
  };

  mkHackageFromSpec = name: extraHackagePackages: rec {
    extra-hackage-tarball = mkHackageTarball name extraHackagePackages;
    extra-hackage = mkHackageNix name extra-hackage-tarball;
    module = mkModule extraHackagePackages;
  };

in
{ srcs # : [string]
, name # : string
}:

if builtins.length srcs == 0
then {
  modules = [ ];
  extra-hackage-tarballs = { };
  extra-hackages = [ ];
}
else
  let
    hackage = mkHackageFromSpec name (map mkPackageSpec srcs);
  in
  {
    modules = [ hackage.module ];
    extra-hackage-tarballs = {
      "${name}-hackage-tarball" = hackage.extra-hackage-tarball;
    };
    extra-hackages = [ (import hackage.extra-hackage) ];
  }
