# overlays/awesome-git/default.nix
{inputs, ...}: final: prev: let
  src = inputs.awesome-master;
in {
  awesome-git = prev.awesome.overrideAttrs (old: {
    pname = "awesome-git";
    version = "git-${src.shortRev or "dirty"}";
    inherit src;

    patches = [];
    postPatch = "";

    cmakeFlags =
      (old.cmakeFlags or [])
      ++ [
        "-DGENERATE_DOC=OFF"
      ];

    meta =
      (old.meta or {})
      // {
        description = "AwesomeWM built from upstream master";
      };
  });
}
