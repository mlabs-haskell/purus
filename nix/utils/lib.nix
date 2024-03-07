{ stdenv
, lib
}:

let
  applyPatches = args @ { patches, ... }: stdenv.mkDerivation ({
    inherit patches;

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      mkdir -p "$out"
      cp -r * "$out"
    '';

    dontFixup = true;
  } // args);

  mkFlag = flag: value: "--${flag}=${value}";

  mkFlags = flag: values: builtins.concatStringsSep " " (map (mkFlag flag) values);

  mkCli = args:
    builtins.concatStringsSep " "
      (lib.attrsets.mapAttrsToList
        (flag: value:
          if builtins.isList value
          then mkFlags flag value
          else if builtins.isBool value then (if value then "--${flag}" else "")
          else mkFlag flag "${value}"
        )
        args);

  withNameAttr = f: name: args: f (args // { inherit name; });
in
{
  inherit applyPatches mkCli withNameAttr;
}
