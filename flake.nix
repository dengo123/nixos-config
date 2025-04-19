{
  description = "";

  inputs = {
    # NixPkgs (nixos-24.05)
    stable.url = "github:nixos/nixpkgs/nixos-24.11";

    # NixPkgs Unstable (nixos-unstable)
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    ### Additional Inputs ###

    # ags
    ags.url = "github:Aylur/ags";

    # Home Manager (release-24.05)
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "unstable";
    };

    # Hardware Configuration
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # Run unpatched dynamically compiled binaries
    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "unstable";
    };

    # Snowfall Lib
    snowfall-lib = {
      url = "github:snowfallorg/lib";
      inputs.nixpkgs.follows = "unstable";
    };

    # Hyprland
    hyprland.url = "github:hyprwm/Hyprland";

    # Apple font
    # apple-fonts.url = "github:Lyndeno/apple-fonts.nix";

    # Sops secrets management
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "unstable";
    };

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "unstable";
    };
  };

  outputs = inputs: let
    lib = inputs.snowfall-lib.mkLib {
      inherit inputs;
      src = ./.;

      snowfall = {
        meta = {
          name = "nixforge";
          title = "The Nix Forge - In the crucible of code, systems awaken. From metal and magic, they arise.";
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
        permittedInsecurePackages = [];
      };

      overlays = with inputs; [];

      systems.modules.nixos = with inputs; [];

      templates = import ./templates {};
    };
}
