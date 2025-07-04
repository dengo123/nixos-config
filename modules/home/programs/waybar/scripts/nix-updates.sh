#!/usr/bin/env bash

cd ~/nixos-config || exit 1

output=$(nix flake update --dry-run 2>&1)

if echo "$output" | grep -q "will be updated"; then
  echo '{"text": "", "tooltip": "Updates verfügbar!"}'
else
  echo '{"text": "", "tooltip": "System aktuell."}'
fi

