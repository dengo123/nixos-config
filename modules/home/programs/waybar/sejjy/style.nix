let
  theme = import ./theme.nix {inherit config lib pkgs;};
in ''
  @define-color crust ${theme.colors.crust};
  @define-color mantle ${theme.colors.mantle};
  @define-color surface0 ${theme.colors.surface0};
  @define-color surface1 ${theme.colors.surface1};
  @define-color surface2 ${theme.colors.surface2};
  @define-color text ${theme.colors.text};
  @define-color rosewater ${theme.colors.rosewater};
  @define-color lavender ${theme.colors.lavender};

  @define-color red ${theme.colors.red};
  @define-color peach ${theme.colors.peach};
  @define-color yellow ${theme.colors.yellow};
  @define-color green ${theme.colors.green};
  @define-color teal ${theme.colors.teal};
  @define-color blue ${theme.colors.blue};
  @define-color mauve ${theme.colors.mauve};
  @define-color flamingo ${theme.colors.flamingo};

  @define-color white ${theme.colors.white};
  @define-color black ${theme.colors.black};

  /* Semantic Colors */
  @define-color shadow ${theme.colors.shadow};
  @define-color main-fg ${theme.colors.main-fg};
  @define-color main-bg ${theme.colors.main-bg};
  @define-color main-br ${theme.colors.main-br};
  @define-color active-bg ${theme.colors.active-bg};
  @define-color active-fg ${theme.colors.active-fg};
  @define-color hover-bg ${theme.colors.hover-bg};
  @define-color hover-fg ${theme.colors.hover-fg};

  @define-color module-fg ${theme.colors.module-fg};
  @define-color workspaces ${theme.colors.workspaces};
  @define-color temperature ${theme.colors.temperature};
  @define-color memory ${theme.colors.memory};
  @define-color cpu ${theme.colors.cpu};
  @define-color distro-fg ${theme.colors.distro-fg};
  @define-color distro-bg ${theme.colors.distro-bg};
  @define-color time ${theme.colors.time};
  @define-color date ${theme.colors.date};
  @define-color tray ${theme.colors.tray};
  @define-color pulseaudio ${theme.colors.pulseaudio};
  @define-color backlight ${theme.colors.backlight};
  @define-color battery ${theme.colors.battery};
  @define-color power ${theme.colors.power};

  @define-color warning ${theme.colors.warning};
  @define-color critical ${theme.colors.critical};
  @define-color charging ${theme.colors.charging};

    /* ───────────────────────────────────────────────────────────────┤ global ├─── */
    * {
      min-height: 0;
      border: none;
      margin: 0;
      padding: 0;
    }

    /* ──────────────────────────────────────────────────────────┤ drop shadow ├─── */
    window#waybar {
      background: @shadow;
    }

    /* ───────────────────────────────────────────────────────────┤ background ├─── */
    window#waybar > box {
      background: @main-bg;
      margin: 2px;
    }

    /* ─────────────────────────────────────────────────────────────┤ tooltips ├─── */
    tooltip {
      background: @main-bg;
      border: 1.5px solid @main-br;
      border-radius: 8px;
    }

    tooltip label {
      color: @main-fg;
      margin: -1.5px 3px;
    }

      /* ──────────────────────────────────────────────────────────────┤ general ├─── */
    #custom-launcher,
    #workspaces,
    #window,
    #custom-temperature,
    #memory,
    #cpu,
    #custom/inhibitor,
    #clock,
    #network,
    #bluetooth,
    #custom-update,
    #mpris,
    #pulseaudio,
    #custom-swaync,
    #custom-nightmode,
    #custom-power {
      opacity: 1;
      color: @module-fg;
      padding: 0 4px;
    }

    #custom-left1,
    #custom-left2,
    #custom-left3,
    #custom-left4,
    #custom-left5,
    #custom-left6,
    #custom-left7,
    #custom-left8 {
      margin-bottom: 0;
      text-shadow: -2px 0 2px rgba(0, 0, 0, 0.5);
    }

    #custom-right1,
    #custom-right2,
    #custom-right3,
    #custom-right4,
    #custom-right5 {
      margin-bottom: 0;
      padding-right: 3px;
      text-shadow: 2px 0 2px rgba(0, 0, 0, 0.5);
    }

      /* ───────────────────────────────────────────────────────┤ launcher button ├─── */
    #custom-launcher {
      background: @main-bg;
    }

    #custom-launcher:hover {
      color: @hover-fg;
    }

      /* ───────────────────────────────────────────────────────────┤ workspaces ├─── */
    #custom-left1 {
      color: @workspaces;
      background: @main-bg;
      margin-bottom: 0;
      padding-left: 2px;
    }

    #workspaces {
      background: @workspaces;
    }

    #workspaces button {
      color: @module-fg;
      border-radius: 8px;
      box-shadow: none;
      margin: 2px 0;
      padding: 0 2px;
      transition: none;
    }

    #workspaces button:hover {
      color: @hover-fg;
      background: @hover-bg;
      text-shadow: none;
    }

    #workspaces button.active {
      color: @active-fg;
      background: @active-bg;
      text-shadow: 0 0 2px rgba(0, 0, 0, 0.6);
      box-shadow: 0 0 2px 1px rgba(0, 0, 0, 0.4);
      margin: 2px;
      padding: 0 6px;
    }

    #custom-right1 {
      color: @workspaces;
      background: @main-bg;
      text-shadow: 3px 0 2px rgba(0, 0, 0, 0.4);
      margin-bottom: 0;
    }

      /* ──────────────────────────────────────────────────────────┤ temperature ├─── */
    #custom-paddc {
      padding-right: 22px;
    }

    #custom-left2 {
      color: @temperature;
      background: @main-bg;
      padding-left: 3px;
    }

    #custom-temperature {
      background: @temperature;
      padding: 0 0 0 1px;
    }

      /* ───────────────────────────────────────────────────────────────┤ memory ├─── */
    #custom-left3 {
      color: @memory;
      background: @temperature;
      padding-left: 3px;
    }

    #memory {
      background: @memory;
      padding: 0 0 0 1px;
    }

    #memory.warning {
      color: @warning;
    }

    #memory.critical {
      color: @critical;
    }

      /* ──────────────────────────────────────────────────────────────────┤ cpu ├─── */
    #custom-left4 {
      color: @cpu;
      background: @memory;
      padding-left: 3px;
    }

    #cpu {
      background: @cpu;
    }

    #custom-leftin1 {
      color: @cpu;
      margin-bottom: -1px;
    }

      /* ──────────────────────────────────────────────────────────┤ distro icon ├─── */
    #custom-left5 {
      color: @distro-bg;
      background: @main-bg;
      text-shadow: none;
      margin-bottom: -2px;
      padding-left: 3px;
    }

    #custom-distro {
      color: @distro-fg;
      background: @distro-bg;
      margin: 0 -1px -2px 0;
      padding: 0 0 0 3px;
      text-shadow: 0 0 1.5px rgba(0, 0, 0, 1);
    }

    #custom-right2 {
      color: @distro-bg;
      background: @main-bg;
      text-shadow: none;
      margin-bottom: -2px;
    }

      /* ─────────────────────────────────────────────────────────────────┤ time ├─── */
    #custom-rightin1 {
      color: @time;
      margin-bottom: -1px;
    }

    #custom-inhibitor {
      background: @time;
      padding: 0 0 0 7px;
    }

    #custom-inhibitor:hover {
      color: @hover-fg;
    }

    #clock.time {
      background: @time;
      margin-left: -2px;
      padding: 0 3px 0 0;
    }

    #custom-right3 {
      color: @time;
      background: @date;
    }

      /* ─────────────────────────────────────────────────────────────────┤ date ├─── */
    #clock.date {
      background: @date;
    }

    #clock.date:hover {
      color: @hover-fg;
    }

    #custom-right4 {
      color: @date;
      background: @tray;
    }

      /* ────────────────────────────────────────────────────────────────┤ wi-fi ├─── */
    #network {
      background: @tray;
      padding: 0 8px 0 5px;
    }

    #network:hover {
      color: @hover-fg;
    }

    /* ────────────────────────────────────────────────────────────┤ bluetooth ├─── */
    #bluetooth {
      background: @tray;
      padding-right: 5px;
    }

    #bluetooth:hover {
      color: @hover-fg;
    }

      /* ────────────────────────────────────────────────────────┤ system update ├─── */
    #custom-update {
      padding-right: 8px;
      background: @tray;
    }

    #custom-update:hover {
      color: @hover-fg;
    }

    #custom-right5 {
      color: @tray;
      background: @main-bg;
    }

      /* ───────────────────────────────────────────────────────────┤ media info ├─── */
    #mpris {
      background: @main-bg;
      padding: 0 8px 0;
    }

    #mpris:hover {
      color: @hover-fg;
    }

      /* ──────────────────────────────────────────────────────┤ output device ├─── */
    #custom-left6 {
      color: @pulseaudio;
      background: @main-bg;
      padding-left: 3px;
    }

    #pulseaudio {
      background: @pulseaudio;
    }

    #pulseaudio:hover {
      color: @hover-fg;
    }

      /* ─────────────────────────────────────────────────────────┤ nightmode ├─── */
    #custom-left7 {
      color: @backlight;
      background: @pulseaudio;
      padding-left: 2px;
    }

    #custom-nightmode {
      background: @backlight;
    }

    #custom-nightmode:hover {
      color: @hover-fg;
    }

      /* ──────────────────────────────────────────────────────────────┤ swaync ├─── */
    #custom-left8 {
      color: @battery;
      background: @backlight;
      padding-left: 2px;
    }

    #custom-swaync {
      background: @battery;
      color: @module-fg;
    }

    #custom-swaync:hover {
      color: @hover-fg;
    }

    /* ─────────────────────────────────────────────────────────┤ power button ├─── */
    #custom-leftin2 {
      color: @battery;
      background: @main-bg;
      margin-bottom: -1px;
    }

    #custom-power {
      color: @main-bg;
      background: @power;
      text-shadow: 0 0 2px rgba(0, 0, 0, 0.6);
      box-shadow: 0 0 2px 1px rgba(0, 0, 0, 0.6);
      border-radius: 10px;
      margin: 2px 4px 2px 0;
      padding: 0 6px 0 9px;
    }

    #custom-power:hover {
      color: @hover-fg;
      background: @hover-bg;
      text-shadow: none;
      box-shadow: none;
    }

      /* ───────────────────────────────────────────────────────────┤ font sizes ├─── */
    * {
      font-family: "JetBrainsMono Nerd Font";
      font-size: 10px;
      font-weight: bold;
    }

    tooltip label,
    #window label,
    #mpris {
      font-weight: normal;
    }

      /* ──────────────────────────────────────────────────┤ left & right arrows ├─── */
    #custom-left1,
    #custom-left2,
    #custom-left3,
    #custom-left4,
    #custom-left5,
    #custom-left6,
    #custom-left7,
    #custom-left8,
    #custom-right1,
    #custom-right2,
    #custom-right3,
    #custom-right4,
    #custom-right5 {
      font-size: 14.68px;
    }

    /* ─────────────────────────────────────────────────┤ left & right inverse ├─── */
    #custom-leftin1,
    #custom-leftin2,
    #custom-rightin1 {
      font-size: 15.5px;
    }

      /* ──────────────────────────────────────────────────────────┤ distro icon ├─── */
    #custom-distro {
      font-size: 14.6px;
    }

    #custom-left5,
    #custom-right2 {
      font-size: 15.68px;
    }

      /* ───────────────────────────────────────────────────────────┤ workspaces ├─── */
    #workspaces button {
      border-radius: 8px;
      padding: 0 2px;
    }

    #workspaces button.active {
      padding: 0 6px;
    }

      /* ─────────────────────────────────────────────────────────┤ power button ├─── */
    #custom-power {
      border-radius: 10px;
      padding: 0 6px 0 9px;
    }

''
