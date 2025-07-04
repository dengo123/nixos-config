{
  layer = "top";
  position = "top";
  mode = "dock";
  gtk-layer-shell = true;

  modules-left = [
    "custom/launcher"
    "custom/left1"
    "hyprland/workspaces"
    "custom/right1"
    "custom/paddw"
    "hyprland/window"
  ];

  modules-center = [
    "custom/paddc"
    "custom/left2"
    "custom/temperature"
    "custom/left3"
    "memory"
    "custom/left4"
    "cpu"
    "custom/leftin1"
    "custom/left5"
    "custom/distro"
    "custom/right2"
    "custom/rightin1"
    "custom/inhibitor"
    "clock#time"
    "custom/right3"
    "clock#date"
    "custom/right4"
    "network"
    "bluetooth"
    "custom/update"
    "custom/right5"
  ];

  modules-right = [
    "mpris"
    "custom/left6"
    "pulseaudio"
    "custom/left7"
    "backlight"
    "custom/left8"
    "custom/swaync"
    "custom/leftin2"
    "custom/power"
  ];

  "custom/launcher" = {
    format = "󰍜 ";
    on-click = "launcher";
    tooltip = false;
  };

  "hyprland/workspaces" = {
    on-scroll-up = "hyprctl dispatch workspace -1";
    on-scroll-down = "hyprctl dispatch workspace +1";
    format = "○";
    format-visible = "●";
    format-active = "<span color='#f5c2e7'></span>";
    persistent-workspaces = {
      "1" = [];
      "2" = [];
      "3" = [];
      "4" = [];
      "5" = [];
    };
  };

  "hyprland/window" = {
    format = "{}";
    tooltip = false;
    min-length = 5;
    rewrite = {
      "" = "<span foreground='#89b4fa'> </span> Hyprland";
      "~" = "  Terminal";
      "zsh" = "  Terminal";
      "kitty" = "  Terminal";
      "ghostty" = "  Terminal";
      "tmux(.*)" = "<span foreground='#a6e3a1'> </span> Tmux";

      "(.*)Mozilla Firefox" = "<span foreground='#f38ba8'>󰈹 </span> Firefox";
      "(.*) — Mozilla Firefox" = "<span foreground='#f38ba8'>󰈹 </span> $1";
      "(.*)Zen Browser" = "<span foreground='#fab387'>󰺕 </span> Zen Browser";
      "(.*) — Zen Browser" = "<span foreground='#fab387'>󰺕 </span> $1";
      "(.*)Brave" = "<span foreground='#fab387'>󰤶 </span> Brave";

      "nvim" = "<span foreground='#a6e3a1'> </span> Neovim";
      "nvim (.*)" = "<span foreground='#a6e3a1'> </span> $1";
      "hx" = "<span foreground='#89b4fa'> </span> Helix";
      "emacs" = "<span foreground='#89b4fa'> </span> Emacs";

      "(.*)Spotify" = "<span foreground='#a6e3a1'> </span> Spotify";
      "(.*)Spotify Premium" = "<span foreground='#a6e3a1'> </span> Spotify Premium";
      "OBS(.*)" = "<span foreground='#a6adc8'>󰻃 </span> OBS Studio";
      "VLC media player" = "<span foreground='#fab387'>󰕼 </span> VLC Media Player";
      "(.*) - VLC media player" = "<span foreground='#fab387'>󰕼 </span> $1";
      "(.*) - mpv" = "<span foreground='#cba6f7'> </span> $1";
      "qView" = "󰋩  qView";

      "(.*).jpg" = "󰋩  $1.jpg";
      "(.*).png" = "󰋩  $1.png";
      "(.*).svg" = "󰋩  $1.svg";

      "• Discord(.*)" = "Discord$1";
      "(.*)Discord(.*)" = "<span foreground='#89b4fa'> </span> $1Discord$2";
      "vesktop" = "<span foreground='#89b4fa'> </span> Discord";

      "libreoffice" = "<span foreground='#f38ba8'> </span> LibreOffice";
      "(.*).docx" = "<span foreground='#89b4fa'>󰈭 </span> $1.docx";
      "(.*).xlsx" = "<span foreground='#a6e3a1'>󰈜 </span> $1.xlsx";
      "(.*).pptx" = "<span foreground='#fab387'>󰈨 </span> $1.pptx";
      "(.*).pdf" = "<span foreground='#f38ba8'> </span> $1.pdf";

      "Authenticate" = "  Authenticate";
    };
  };

  "custom/temperature" = {
    exec = "~/nixos-config/modules/home/programs/waybar/scripts/cpu-temp.sh";
    return-type = "json";
    format = "{}";
    interval = 5;
    min-length = 8;
    max-length = 8;
  };

  "memory" = {
    states = {
      warning = 75;
      critical = 90;
    };

    format = "󰘚 {percentage}%";
    format-critical = "󰀦 {percentage}%";
    tooltip = false;
    interval = 5;
    min-length = 7;
    max-length = 7;
  };

  "cpu" = {
    format = "󰍛 {usage}%";
    tooltip = false;
    interval = 5;
    min-length = 6;
    max-length = 6;
  };

  "custom/distro" = {
    format = " ";
    tooltip = false;
  };

  "custom/inhibitor" = {
    exec = "~/nixos-config/modules/home/programs/waybar/scripts/idle-toggle.sh";
    return-type = "json";
    interval = 2;
    tooltip = true;
    on-click = "~/nixos-config/modules/home/programs/waybar/scripts/idle-toggle.sh toggle";
  };

  "clock#time" = {
    format = "{:%H:%M}";
    interval = 60;
    tooltip = false;
    min-length = 6;
    max-length = 6;
  };

  "clock#date" = {
    format = "{:%A, %d.%m.}";
    tooltip-format = "<tt>{calendar}</tt>";
    interval = 60;

    calendar = {
      mode = "month";
      mode-mon-col = 6;
      format = {
        months = "<span color='#b4befe'><b>{}</b></span>";
        weekdays = "<span color='#a6adc8' font='7'>{}</span>";
        today = "<span color='#f38ba8'><b>{}</b></span>";
      };
    };

    actions = {
      on-click = "mode";
      on-click-right = "mode";
    };

    min-length = 18;
    max-length = 18;
  };

  "network" = {
    format-wifi = "󰖩";
    format-ethernet = "󰈀";
    format-disconnected = "󰖪";
    tooltip-format = "{ipaddr} ({signalStrength}%) via {essid}";
    on-click = "ghostty --title '󰤨  Network Manager TUI' bash -c nmtui";
  };

  "bluetooth" = {
    format = "󰂯";
    format-disabled = "󰂲";
    format-connected = "󰂱";
    format-connected-battery = "󰂱";
    interval = 10;

    tooltip-format = "{num_connections} connected";
    tooltip-format-disabled = "Bluetooth Disabled";
    tooltip-format-connected = "{device_enumerate}";
    tooltip-format-enumerate-connected = "{device_alias}";
    tooltip-format-enumerate-connected-battery = ":: {device_alias}: 󱊣 {device_battery_percentage}%";

    on-click = "ghostty --title '󰂯  Bluetooth TUI' bash -c bluetui";
  };

  "custom/update" = {
    exec = "~/nixos-config/modules/home/programs/waybar/scripts/nix-updates.sh";
    return-type = "json";
    interval = 300; # alle 5 Minuten
    on-click = "ghostty --title 'Update' bash -c 'cd ~/nixos-config && nix flake update'";
    tooltip = true;
  };

  "mpris" = {
    format = "{player_icon} {title} - {artist}";
    format-paused = "{status_icon} {title} - {artist}";

    player-icons = {
      default = "󰝚 ";
      spotify = "<span foreground='#a6e3a1'>󰓇 </span>";
      firefox = "<span foreground='#f38ba8'>󰗃 </span>";
    };

    status-icons = {
      paused = "<span color='#b4befe'>\u200A\u200A󰏤\u2009\u2009</span>";
    };

    tooltip-format = "Playing: {title} - {artist}";
    tooltip-format-paused = "Paused: {title} - {artist}";

    min-length = 5;
    max-length = 35;
  };

  "pulseaudio" = {
    format = "{icon} {volume}%";
    format-muted = "󰝟";
    format-icons = {
      default = [
        "󰕿"
        "󰖀"
        "󰕾"
      ];
    };
    on-click = "pamixer -t";
    on-scroll-up = "pamixer -i 1";
    on-scroll-down = "pamixer -d 1";
    tooltip = false;
  };

  "custom/nightmode" = {
    exec = "~/nixos-config/modules/home/programs/waybar/scripts/nightmode.sh";
    on-click = "~/nixos-config/modules/home/programs/waybar/scripts/nightmode.sh toggle";
    return-type = "json";
    interval = 5;
    tooltip = false;
  };

  "custom/swaync" = {
    exec = ''
      swaync-client -D | grep -q true && echo '{"text": "", "class": "dnd-active"}' || echo '{"text": "󰂛", "class": "dnd-inactive"}'
    '';
    return-type = "json";
    format = "{}";
    interval = 2;
    on-click = "swaync-client -t -sw";
    tooltip = false;
  };

  "custom/power" = {
    format = " ";
    tooltip = false;
    on-click = "wlogout";
  };

  "custom/paddw" = {
    format = " ";
    tooltip = false;
  };

  "custom/paddc" = {
    format = " ";
    tooltip = false;
  };

  "custom/left1" = {
    format = "";
    tooltip = false;
  };

  "custom/left2" = {
    format = "";
    tooltip = false;
  };

  "custom/left3" = {
    format = "";
    tooltip = false;
  };

  "custom/left4" = {
    format = "";
    tooltip = false;
  };

  "custom/left5" = {
    format = "";
    tooltip = false;
  };

  "custom/left6" = {
    format = "";
    tooltip = false;
  };

  "custom/left7" = {
    format = "";
    tooltip = false;
  };

  "custom/left8" = {
    format = "";
    tooltip = false;
  };

  "custom/right1" = {
    format = "";
    tooltip = false;
  };

  "custom/right2" = {
    format = "";
    tooltip = false;
  };

  "custom/right3" = {
    format = "";
    tooltip = false;
  };

  "custom/right4" = {
    format = "";
    tooltip = false;
  };

  "custom/right5" = {
    format = "";
    tooltip = false;
  };

  "custom/leftin1" = {
    format = "";
    tooltip = false;
  };

  "custom/leftin2" = {
    format = "";
    tooltip = false;
  };

  "custom/rightin1" = {
    format = "";
    tooltip = false;
  };
}
