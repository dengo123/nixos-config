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

  whisperPkg =
    if cfg.backend == "cuda" then
      pkgs.whisper-cpp-cuda
    else if cfg.backend == "vulkan" then
      pkgs.whisper-cpp-vulkan
    else
      pkgs.whisper-cpp;

  whisperCli = "${whisperPkg}/bin/whisper-cli";

  transcribe = pkgs.writeShellScriptBin "whisper-transcribe-trading" ''
    set -euo pipefail
    shopt -s globstar nullglob

    ROOT="${cfg.rootDir}"
    MODEL="${cfg.modelPath}"

    if [[ -z "$MODEL" ]]; then
      echo "ERROR: set ${namespace}.programs.whisper.modelPath (e.g. ~/models/whispercpp/ggml-large-v3.bin)"
      exit 1
    fi
    if [[ ! -f "$MODEL" ]]; then
      echo "ERROR: model not found: $MODEL"
      exit 1
    fi

    for in in "$ROOT"/**/*.mp4; do
      echo "==> $in"
      wav="''${in%.mp4}.wav"

      ${pkgs.ffmpeg}/bin/ffmpeg -y -i "$in" -vn -ac 1 -ar 16000 -c:a pcm_s16le "$wav" >/dev/null 2>&1

      ${whisperCli} \
        -m "$MODEL" \
        -f "$wav" \
        -l ${cfg.language} \
        ${optionalString cfg.writeTxt "-otxt"} \
        ${optionalString cfg.writeSrt "-osrt"} \
        ${optionalString cfg.writeVtt "-ovtt"} \
        -of "''${in%.mp4}" \
        ${concatStringsSep " " cfg.extraArgs}

      rm -f "$wav"
    done
  '';
in
{
  options.${namespace}.programs.whisper = with types; {
    enable = mkBoolOpt false "Install whisper-cli + ffmpeg + batch script for Trading videos";
    backend = mkOpt (enum [
      "cpu"
      "vulkan"
      "cuda"
    ]) "cpu" "Select whisper backend";
    rootDir = mkOpt str "/home/dengo123/Documents/Trading" "Root dir containing mp4 files";
    modelPath = mkOpt str "" "Path to ggml model file";
    language = mkOpt str "en" "Spoken language ('en' recommended)";
    writeTxt = mkBoolOpt true "Write .txt";
    writeSrt = mkBoolOpt true "Write .srt";
    writeVtt = mkBoolOpt true "Write .vtt";
    extraArgs = mkOpt (listOf str) [ "--print-progress" ] "Extra whisper-cli args";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      whisperPkg
      pkgs.ffmpeg
      transcribe
    ];
  };
}
