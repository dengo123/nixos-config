# overlays/sddm-themes/default.nix
{...}: final: prev: {
  sddm-theme-winxpconcept =
    final.callPackage ../../packages/sddm-winxpconcept {};
}
