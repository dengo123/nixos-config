{...}: {
  lib = {
    description = "Template for a NixOS-compatible library module";
    path = ./lib;
  };

  module = {
    description = "Template for a Snowfall-compatible NixOS module";
    path = ./module;
  };

  overlay = {
    description = "Template for a nixpkgs overlay";
    path = ./overlay;
  };

  system = {
    description = "Basic system configuration template";
    path = ./system;
  };

  home = {
    description = "Basic Home Manager configuration template";
    path = ./home;
  };
}
