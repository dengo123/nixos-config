{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.waybar;

  brightnessctl = pkgs.brightnessctl + "/bin/brightnessctl";
  pamixer = pkgs.pamixer + "/bin/pamixer";
  waybar-wttr = pkgs.stdenv.mkDerivation {
    name = "waybar-wttr";
    buildInputs = [(pkgs.python310.withPackages (pythonPackages: with pythonPackages; [requests]))];
    unpackPhase = "true";
    installPhase = ''
      mkdir -p $out/bin
      cp ${./scripts/waybar-wttr.py} $out/bin/waybar-wttr
      chmod +x $out/bin/waybar-wttr
    '';
  };
in {
  options.${namespace}.desktop.waybar = {
    enable = mkBoolOpt false "${namespace}.programs.waybar.enable";
  };

  config = mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          mode = "dock";
          exclusive = true;
          passthrough = false;
          fixed-center = true;
          gtk-layer-shell = true;
          height = 34;
          modules-left = [
            "custom/logo"
            "hyprland/workspaces"
            "custom/weather"
            "custom/todo"
            "tray"
          ];

          modules-center = [];

          modules-right = [
            "battery"
            "backlight"
            "pulseaudio#microphone"
            "pulseaudio"
            "network"
            "clock#date"
            "clock"
            "custom/swaync"
            "custom/power"
          ];

          "hyprland/workspaces" = {
            on-click = "activate";
            format = "{name}";
            all-outputs = true;
            disable-scroll = true;
            active-only = false;
          };

          "custom/logo" = {
            tooltip = true;
            on-click = "rofi -show drun";
            format = " ";
          };

          "custom/todo" = {
            tooltip = true;
            format = "{}";
            interval = 7;
            exec = let
              todo = pkgs.todo + "/bin/todo";
              sed = pkgs.gnused + "/bin/sed";
              wc = pkgs.coreutils + "/bin/wc";
            in
              pkgs.writeShellScript "todo-waybar" ''
                #!/bin/sh

                total_todo=$(${todo} | ${wc} -l)
                todo_raw_done=$(${todo} raw done | ${sed} 's/^/      ◉ /' | ${sed} -z 's/\n/\\n/g')
                todo_raw_undone=$(${todo} raw todo | ${sed} 's/^/     ◉ /' | ${sed} -z 's/\n/\\n/g')
                done=$(${todo} raw done | ${wc} -l)
                undone=$(${todo} raw todo | ${wc} -l)
                tooltip=$(${todo})

                left="$done/$total_todo"

                header="<b>todo</b>\\n\\n"
                tooltip=""
                if [[ $total_todo -gt 0 ]]; then
                	if [[ $undone -gt 0 ]]; then
                		export tooltip="$header👷 Today, you need to do:\\n\\n $(echo $todo_raw_undone)\\n\\n✅ You have already done:\\n\\n $(echo $todo_raw_done)"
                		export output=" 🗒️ $left"
                	else
                		export tooltip="$header✅ All done!\\n🥤 Remember to stay hydrated!"
                		export output=" 🎉 $left"
                	fi
                else
                	export tooltip=""
                	export output=""
                fi

                printf '{"text": "%s", "tooltip": "%s" }' "$output" "$tooltip"
              '';
            return-type = "json";
          };

          "custom/weather" = {
            tooltip = true;
            format = "{}";
            interval = 30;
            exec = "${waybar-wttr}/bin/waybar-wttr";
            return-type = "json";
          };

          "custom/power" = {
            tooltip = false;
            on-click = "power-menu";
            format = "󰤆";
          };

          tray = {
            spacing = 10;
          };

          "custom/swaync" = {
            exec = ''
              if swaync-client -D | grep -q true; then
                echo -n '{"text": "", "tooltip": "Do Not Disturb", "class": "dnd-active"}'
              else
                echo -n '{"text": "", "tooltip": "", "class": "dnd-inactive"}'
              fi
            '';
            return-type = "json";
            format = "{} ";
            interval = 2;
            on-click = "swaync-client -t -sw";
            escape = true;
            tooltip = false;
          };

          clock = {
            tooltip = false;
            format = "󱑎 {:%H:%M}";
          };

          "clock#date" = {
            format = "󰃶 {:%a %d %b}";
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
          };

          backlight = {
            tooltip = false;
            format = "{icon} {percent}%";
            format-icons = [
              "󰋙"
              "󰫃"
              "󰫄"
              "󰫅"
              "󰫆"
              "󰫇"
              "󰫈"
            ];
            on-scroll-up = "${brightnessctl} s 1%-";
            on-scroll-down = "${brightnessctl} s +1%";
          };

          battery = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = "{icon} {capacity}%";
            tooltip-format = "{timeTo}, {capacity}%";
            format-charging = "󰂄 {capacity}%";
            format-plugged = "󰚥 {capacity}%";
            format-alt = "{time} {icon}";
            format-icons = [
              "󰂃"
              "󰁺"
              "󰁻"
              "󰁼"
              "󰁽"
              "󰁾"
              "󰁿"
              "󰂀"
              "󰂁"
              "󰂂"
              "󰁹"
            ];
          };

          network = {
            format-wifi = "󰖩 {essid}";
            format-ethernet = "󰈀 {ipaddr}/{cidr}";
            format-alt = "󱛇";
            format-disconnected = "󰖪";
            tooltip-format = ''
              󰅃 {bandwidthUpBytes} 󰅀 {bandwidthDownBytes}
              {ipaddr}/{ifname} via {gwaddr} ({signalStrength}%)'';
          };

          pulseaudio = {
            tooltip = false;
            format = "{icon} {volume}%";
            format-muted = "󰖁";
            format-icons = {
              default = [
                "󰕿"
                "󰖀"
                "󰕾"
              ];
            };
            tooltip-format = "{desc}, {volume}%";
            on-click = "${pamixer} -t";
            on-scroll-up = "${pamixer} -d 1";
            on-scroll-down = "${pamixer} -i 1";
          };

          "pulseaudio#microphone" = {
            tooltip = false;
            format = "{format_source}";
            format-source = "󰍬 {volume}%";
            format-source-muted = "󰍭";
            on-click = "${pamixer} --default-source -t";
            on-scroll-up = "${pamixer} --default-source -d 1";
            on-scroll-down = "${pamixer} --default-source -i 1";
          };
        };
      };
      systemd.enable = true;
    };

    xdg.configFile."waybar/style.css".text = import ./style.nix;
  };
}
