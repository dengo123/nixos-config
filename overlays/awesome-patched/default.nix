# overlays/awesome-patched/default.nix
{inputs, ...}: final: prev: let
  src = inputs.awesome-master;
in {
  awesome-git =
    (prev.awesome.override {
      lua = final.lua5_2.withPackages (ps: [
        ps.lgi
        ps.cjson
      ]);
    }).overrideAttrs (old: {
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
          description = "AwesomeWM built from upstream master";
        };
    });
}
