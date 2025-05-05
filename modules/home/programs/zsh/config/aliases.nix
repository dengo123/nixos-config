{
  cl = "clear";

  ".." = "cd ..";
  "..." = "cd ../..";
  "...." = "cd ../../..";
  "....." = "cd ../../../..";

  grep = "grep --color=auto";

  update = "sudo nix flake update --flake ~/nixos-config && sudo nixos-rebuild switch --flake ~/nixos-config";
  switch = "sudo nixos-rebuild switch --flake ~/nixos-config";

  v = "nvim";
  n = "nano";
  e = "emacs";

  r = "reboot";
  p = "poweroff";

  g = "git";
  ga = "git add";
  gaa = "git add --all";
  gp = "git push";
  gl = "git pull";
  gc = "git commit";
  gcm = "git commit -m";
  gcam = "git commit -a -m";
  gr = "git reset";

  zi = "zoxide query --interactive";
  lg = "lazygit";
  ls = "eza";
  ll = "eza -lah";
  tree = "eza --tree";
}
