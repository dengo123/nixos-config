# flake.nix
{
  description = "The NixForge – In the crucible of code, systems awaken";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    snowfall-lib = {
      url = "github:snowfallorg/lib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    awesome-master = {
      url = "github:awesomeWM/awesome/master";
      flake = false;
    };

    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };

    ghostty.url = "github:ghostty-org/ghostty";

    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";
  };

  outputs = inputs: let
    lib = inputs.snowfall-lib.mkLib {
      inherit inputs;
      src = ./.;

      snowfall = {
        meta = {
          name = "nixforge";
          title = "The NixForge – In the crucible of code, systems awaken";
        };
        namespace = "nixforge";
      };
    };
  in
    lib.mkFlake {
      inherit inputs;
      src = ./.;

      channels-config = {
        allowUnfree = true;
      };

      specialArgs = {
        inherit inputs;
        system = "x86_64-linux";
      };

      systems.hosts.anvil = {};

      system.users."dengo123@anvil".modules = with inputs; [
      ];

      templates = import ./templates {};
    };
}
