{
  description = "nixforge - In the crucible of code, systems awaken";

  inputs = {
    # 🧊 Channels
    stable.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # ❄️ Snowfall
    snowfall-lib = {
      url = "github:snowfallorg/lib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # 🏠 Home Manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # 🧪 AGS – Aylur's GTK Shell
    ags.url = "github:Aylur/ags";

    # 🖥 Hyprland
    hyprland.url = "github:hyprwm/Hyprland";

    # 🌐 Zen Browser
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # 🍏 Apple Fonts (optional)
    apple-fonts.url = "github:Lyndeno/apple-fonts.nix";

    # ⚙️ Nix-ld
    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # 🖥 Hardware Support
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    # 📦 NixOS Generators
    nixos-generators.url = "github:nix-community/nixos-generators";
  };

  outputs = inputs: let
    lib = inputs.snowfall-lib.mkLib {
      inherit inputs;
      src = ./.;

      snowfall = {
        meta = {
          name = "nixforge";
          title = "The Nix Forge - In the crucible of code, systems awaken.";
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

      systems.modules.nixos = with inputs; [
      ];

      # 🔧 Aktiver Host
      systems.hosts = {
        anvil = {
          specialArgs = {
            namespace = "nixforge";
            channel = inputs.nixpkgs;
            pkgs = import inputs.nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              overlays = [];
            };
          };
        };

        # furnace = {
        #   specialArgs = {
        #     namespace = "nixforge";
        #     channel = inputs.nixpkgs;
        #     pkgs = import inputs.nixpkgs {
        #       system = x86_64-linux;
        #       config.allowUnfree = true;
        #     };
        #   };
        # };

        # casting = {
        #   specialArgs = {
        #     namespace = "nixforge";
        #     channel = inputs.nixpkgs;
        #     pkgs = import inputs.nixpkgs {
        #       system = x86_64-linux;
        #       config.allowUnfree = true;
        #     };
        #   };
        # };

        # blueprint = {
        #   specialArgs = {
        #     namespace = "nixforge";
        #     channel = inputs.nixpkgs;
        #     pkgs = import inputs.nixpkgs {
        #       system = x86_64-linux;
        #       config.allowUnfree = true;
        #     };
        #   };
        # };
      };

      templates = import ./templates {};
    };
}
