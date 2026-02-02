final: prev: {
  whisper-cpp-cuda = prev.whisper-cpp.overrideAttrs (old: {
    cmakeFlags = (old.cmakeFlags or [ ]) ++ [ "-DGGML_CUDA=1" ];

    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
      prev.cudaPackages.cuda_nvcc
    ];

    buildInputs = (old.buildInputs or [ ]) ++ [
      prev.cudaPackages.cudatoolkit
      prev.cudaPackages.libcublas
    ];
  });
}
