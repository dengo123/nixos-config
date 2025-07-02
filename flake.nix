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

    ags.url = "github:Aylur/ags";
    hyprland.url = "github:hyprwm/Hyprland";

    hyprsplit = {
      url = "github:shezdy/hyprsplit";
      inputs.hyprland.follows = "hyprland";
    };

    nixvim.url = "github:nix-community/nixvim";
    ghostty.url = "github:ghostty-org/ghostty";

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    apple-fonts.url = "github:Lyndeno/apple-fonts.nix";

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
          title = "nixforge – In the crucible of code, systems awaken";
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
