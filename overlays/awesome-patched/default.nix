# overlays/awesome-patched/default.nix
{inputs, ...}: final: prev: let
  src = inputs.awesome-master;
in {
  awesome-patched = prev.awesome.overrideAttrs (old: {
    pname = "awesome-patched";
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
        description = "Patched AwesomeWM built from upstream master";
      };
  });
}
