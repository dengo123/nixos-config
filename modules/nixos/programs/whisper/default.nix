# modules/nixos/programs/whisper/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.whisper;

  # single source of truth: bundles.gpu.vendor
  gpuVendor = config.${namespace}.bundles.gpu.vendor or null;

  backendResolved =
    if cfg.backend != "auto"
    then cfg.backend
    else if gpuVendor == "nvidia"
    then "cuda"
    else if gpuVendor == "amd"
    then "vulkan"
    else "cpu";

  whisperPkg =
    if backendResolved == "cuda"
    then (pkgs.whisper-cpp-cuda or pkgs.whisper-cpp)
    else if backendResolved == "vulkan"
    then pkgs.whisper-cpp-vulkan
    else pkgs.whisper-cpp;

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
  whisperArgs =
    [
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
      findutils
      whisperCliWrapped
    ];

    text = ''
      set -euo pipefail

      if [ $# -lt 1 ]; then
        echo "usage: whisper-transcribe <file> [-- extra whisper args]" >&2
        exit 2
      fi

      in="$1"; shift || true
      if [ "''${1:-}" = "--" ]; then
        shift || true
      fi

      if [ ! -f "$in" ]; then
        echo "whisper-transcribe: file not found: $in" >&2
        exit 1
      fi

      base_root="${cfg.baseRoot}"
      out_root="${cfg.outputRoot}"

      in_abs="$(readlink -f "$in")"
      base_abs="$(readlink -f "$base_root")"

      # Compute path relative to baseRoot
      # If file is outside baseRoot, put it under _outside/...
      rel=""
      case "$in_abs" in
        "$base_abs"/*)
          rel="''${in_abs#"$base_abs"/}"
          ;;
        *)
          rel="_outside/''${in_abs#/}"
          ;;
      esac

      rel_dir="''${rel%/*}"
      name="$(basename "''${in_abs%.*}")"

      out_dir="$out_root/$rel_dir"
      mkdir -p "$out_dir"

      md_out="$out_dir/$name.md"
      tmp_base="$out_dir/$name.whisper_tmp"

      # skip if already exists
      if [ -f "$md_out" ]; then
        echo "skip: $in -> $md_out"
        exit 0
      fi

      # normalize extension for audio passthrough
      ext="''${in_abs##*.}"
      ext_lc="$(printf "%s" "$ext" | tr '[:upper:]' '[:lower:]')"

      tmp_wav=""
      cleanup() { [ -n "$tmp_wav" ] && rm -f "$tmp_wav"; }
      trap cleanup EXIT

      # Whisper produces .txt; we'll rename to .md
      case "$ext_lc" in
        wav|mp3|ogg|flac)
          whisper-cli -m "${cfg.modelPath}" -f "$in_abs" -of "$tmp_base" -l "${cfg.language}" -otxt ${concatStringsSep " " cfg.extraArgs} "$@"
          ;;
        *)
          tmp_wav="$(mktemp --suffix=.wav)"
          ffmpeg -y -i "$in_abs" -vn -ac 1 -ar 16000 -c:a pcm_s16le "$tmp_wav" >/dev/null 2>&1
          whisper-cli -m "${cfg.modelPath}" -f "$tmp_wav" -of "$tmp_base" -l "${cfg.language}" -otxt ${concatStringsSep " " cfg.extraArgs} "$@"
          ;;
      esac

      if [ -f "''${tmp_base}.txt" ]; then
        mv -f "''${tmp_base}.txt" "$md_out"
      else
        echo "whisper-transcribe: expected output missing: ''${tmp_base}.txt" >&2
        exit 1
      fi

      # Cleanup any accidental extras (in case defaults change)
      rm -f "''${tmp_base}".{srt,vtt,lrc,csv,json,json-full} 2>/dev/null || true

      echo "ok: $md_out"
    '';
  };

  # Helper: transcribe a directory tree of mp4 files
  transcribeTree = pkgs.writeShellApplication {
    name = "whisper-transcribe-tree";
    runtimeInputs = with pkgs; [
      findutils
      coreutils
      transcribeOne
    ];

    text = ''
      set -euo pipefail

      if [ $# -lt 1 ]; then
        echo "usage: whisper-transcribe-tree <dir> [-- extra whisper args]" >&2
        exit 2
      fi

      root="$1"; shift || true
      if [ "''${1:-}" = "--" ]; then
        shift || true
      fi

      if [ ! -d "$root" ]; then
        echo "whisper-transcribe-tree: not a directory: $root" >&2
        exit 1
      fi

      # NUL-safe, case-insensitive mp4 search
      while IFS= read -r -d $'\0' f; do
        echo "==> $f"
        whisper-transcribe "$f" -- "$@"
      done < <(find "$root" -type f -iname '*.mp4' -print0)
    '';
  };
in {
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
      mkOpt (listOf str) []
      "Extra args passed to whisper-cli (e.g. [\"-bs\" \"10\" \"-bo\" \"10\"]).";

    outputs = {
      txt = mkBoolOpt true "Write .txt output";
      srt = mkBoolOpt false "Write .srt subtitles";
      vtt = mkBoolOpt false "Write .vtt subtitles";
      lrc = mkBoolOpt false "Write .lrc output";
      csv = mkBoolOpt false "Write .csv output";
      json = mkBoolOpt false "Write .json output";
      jsonFull = mkBoolOpt false "Write .json-full output";
    };

    baseRoot =
      mkOpt path "/home/dengo123/Documents/Trading"
      "Base directory used to compute relative paths for mirrored transcript output.";

    outputRoot =
      mkOpt path "/home/dengo123/Documents/Trading/_recall"
      "Where to write transcripts mirrored relative to baseRoot.";
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
          assertion = nvidiaDrv != null;
          message = "nixforge.programs.whisper: backend=cuda requires config.hardware.nvidia.package to be set (enable NVIDIA drivers).";
        }
      ];
    })
  ]);
}
