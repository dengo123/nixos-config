# modules/nixos/programs/whisper/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.whisper;

  # single source of truth: bundles.gpu.vendor
  gpuVendor = (config.${namespace}.bundles.gpu.vendor or null);

  backendResolved =
    if cfg.backend != "auto" then
      cfg.backend
    else if gpuVendor == "nvidia" then
      "cuda"
    else if gpuVendor == "amd" then
      "vulkan"
    else
      "cpu";

  whisperPkg =
    if backendResolved == "cuda" then
      (pkgs.whisper-cpp-cuda or pkgs.whisper-cpp)
    else if backendResolved == "vulkan" then
      pkgs.whisper-cpp-vulkan
    else
      pkgs.whisper-cpp;

  # NVIDIA driver derivation (contains libcuda.so + libnvidia-ml.so)
  nvidiaDrv = config.hardware.nvidia.package or null;

  # output flags
  outFlags =
    (optional cfg.outputs.txt "-otxt")
    ++ (optional cfg.outputs.srt "-osrt")
    ++ (optional cfg.outputs.vtt "-ovtt")
    ++ (optional cfg.outputs.lrc "-olrc")
    ++ (optional cfg.outputs.csv "-ocsv")
    ++ (optional cfg.outputs.json "-oj")
    ++ (optional cfg.outputs.jsonFull "-ojf");

  # common args to whisper-cli
  whisperArgs = [
    "-l"
    cfg.language
  ]
  ++ (optional (cfg.threads != null) "-t")
  ++ (optional (cfg.threads != null) (toString cfg.threads))
  ++ cfg.extraArgs
  ++ outFlags;

  # We intentionally provide our own wrapped `whisper-cli` in the PATH.
  # For cuda: prepend NVIDIA driver libs so we don't hit the CUDA stub library.
  whisperCliWrapped = pkgs.writeShellScriptBin "whisper-cli" ''
    set -euo pipefail

    ${lib.optionalString (backendResolved == "cuda") ''
      export LD_LIBRARY_PATH="${config.hardware.nvidia.package}/lib:${config.hardware.nvidia.package}/lib64''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
      exec "${pkgs.whisper-cpp-cuda}/bin/whisper-cli" "$@"
    ''}

    ${lib.optionalString (backendResolved == "vulkan") ''
      exec "${pkgs.whisper-cpp-vulkan}/bin/whisper-cli" "$@"
    ''}

    ${lib.optionalString (backendResolved == "cpu") ''
      exec "${pkgs.whisper-cpp}/bin/whisper-cli" "$@"
    ''}

    echo "whisper-cli wrapper: unsupported backend=${backendResolved}" >&2
    exit 1
  '';

  # Helper: transcribe one file (mp4 -> wav -> whisper)
  transcribeOne = pkgs.writeShellApplication {
    name = "whisper-transcribe";
    runtimeInputs = with pkgs; [
      ffmpeg
      coreutils
      whisperCliWrapped
    ];
    text = ''
      set -euo pipefail

      if [ $# -lt 1 ]; then
        echo "usage: whisper-transcribe <file.mp4|file.wav|file.mp3|...> [-- extra whisper args]" >&2
        exit 2
      fi

      in="$1"; shift || true

      # allow "--" to pass args through
      if [ "''${1:-}" = "--" ]; then
        shift
      fi

      if [ ! -f "$in" ]; then
        echo "whisper-transcribe: file not found: $in" >&2
        exit 1
      fi

      out_base="''${in%.*}"

      # If the input is an audio file supported by whisper-cli, pass through.
      # For mp4/mkv/etc: extract 16kHz mono WAV for best results.
      ext="''${in##*.}"
      ext_lc="$(printf "%s" "$ext" | tr '[:upper:]' '[:lower:]')"

      tmp=""
      cleanup() { [ -n "$tmp" ] && rm -f "$tmp"; }
      trap cleanup EXIT

      case "$ext_lc" in
        wav|mp3|ogg|flac)
          whisper-cli -m "${cfg.modelPath}" -f "$in" -of "$out_base" ${concatStringsSep " " whisperArgs} "$@"
          ;;
        *)
          tmp="$(mktemp --suffix=.wav)"
          ffmpeg -y -i "$in" -vn -ac 1 -ar 16000 -c:a pcm_s16le "$tmp" >/dev/null 2>&1
          whisper-cli -m "${cfg.modelPath}" -f "$tmp" -of "$out_base" ${concatStringsSep " " whisperArgs} "$@"
          ;;
      esac
    '';
  };

  # Helper: transcribe a directory tree of mp4 files
  transcribeTree = pkgs.writeShellApplication {
    name = "whisper-transcribe-tree";
    runtimeInputs = with pkgs; [
      findutils
      coreutils
      ffmpeg
      transcribeOne
    ];
    text = ''
      set -euo pipefail

      if [ $# -lt 1 ]; then
        echo "usage: whisper-transcribe-tree <dir> [-- extra whisper args]" >&2
        exit 2
      fi

      root="$1"
      shift || true

      # pass-through args delimiter
      if [ "''${1:-}" = "--" ]; then
        shift || true
      fi

      if [ ! -d "$root" ]; then
        echo "whisper-transcribe-tree: not a directory: $root" >&2
        exit 1
      fi

      # Choose a sentinel extension to decide whether to skip.
      # Prefer txt if enabled, otherwise pick the first enabled output, else default to txt.
      sentinel_ext="txt"
      ${lib.optionalString (!cfg.outputs.txt) ''
        if ${lib.boolToString cfg.outputs.srt}; then sentinel_ext="srt"
        elif ${lib.boolToString cfg.outputs.vtt}; then sentinel_ext="vtt"
        elif ${lib.boolToString cfg.outputs.lrc}; then sentinel_ext="lrc"
        elif ${lib.boolToString cfg.outputs.csv}; then sentinel_ext="csv"
        elif ${lib.boolToString cfg.outputs.json}; then sentinel_ext="json"
        elif ${lib.boolToString cfg.outputs.jsonFull}; then sentinel_ext="json"
        else sentinel_ext="txt"
        fi
      ''}

      # Find mp4 files (case-insensitive) and process them safely with NUL delimiters
      while IFS= read -r -d $'\0' f; do
        # Skip if sentinel output already exists
        out="''${f%.*}.$sentinel_ext"
        if [ -f "$out" ]; then
          echo "skip: $f"
          continue
        fi

        echo "==> $f"
        whisper-transcribe "$f" -- "$@"
      done < <(find "$root" -type f \( -iname '*.mp4' \) -print0)
    '';
  };

in
{
  options.${namespace}.programs.whisper = with types; {
    enable = mkBoolOpt false "Enable whisper.cpp tooling (whisper-cli wrapper + helpers).";

    backend =
      mkOpt
        (enum [
          "auto"
          "cpu"
          "cuda"
          "vulkan"
        ])
        "auto"
        "Which backend to use. auto uses bundles.gpu.vendor (nvidia->cuda, amd->vulkan, else cpu).";

    # We default to a sensible user path. Change it in your host config/bundle.
    modelPath =
      mkOpt path "/home/dengo123/models/whispercpp/ggml-large-v3.bin"
        "Path to a whisper.cpp ggml model file (e.g. ggml-large-v3.bin).";

    language = mkOpt str "en" "Spoken language ('en' or 'auto').";
    threads = mkOpt (nullOr int) null "Thread count for whisper-cli (-t). null = whisper default.";

    extraArgs =
      mkOpt (listOf str) [ ]
        "Extra args passed to whisper-cli (e.g. [\"-bs\" \"10\" \"-bo\" \"10\"]).";

    outputs = {
      txt = mkBoolOpt true "Write .txt output";
      srt = mkBoolOpt true "Write .srt subtitles";
      vtt = mkBoolOpt true "Write .vtt subtitles";
      lrc = mkBoolOpt false "Write .lrc output";
      csv = mkBoolOpt false "Write .csv output";
      json = mkBoolOpt false "Write .json output";
      jsonFull = mkBoolOpt false "Write .json-full output";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Make sure the wrapped whisper-cli wins in PATH:
      environment.systemPackages = [
        whisperCliWrapped
        transcribeOne
        transcribeTree
        pkgs.ffmpeg
      ];
    }

    # Guard rails: if user forces cuda, they should have NVIDIA enabled.
    (mkIf (backendResolved == "cuda") {
      assertions = [
        {
          assertion = (nvidiaDrv != null);
          message = "nixforge.programs.whisper: backend=cuda requires config.hardware.nvidia.package to be set (enable NVIDIA drivers).";
        }
      ];
    })
  ]);
}
