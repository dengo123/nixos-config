# overlays/nvidia-pstated/default.nix
{inputs, ...}: final: prev: {
  nvidia-pstated = inputs.nvidia-pstated.packages.${final.system}.default;
}
