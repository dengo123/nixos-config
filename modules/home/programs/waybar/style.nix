''
  /* Basisfarben – Catppuccin Mocha */
  @define-color crust      #1e1e2e;
  @define-color mantle     #181825;
  @define-color surface0   #313244;
  @define-color surface1   #45475a;
  @define-color surface2   #585b70;
  @define-color text       #cdd6f4;
  @define-color rosewater  #f5e0dc;
  @define-color lavender   #b4befe;

  /* Akzentfarben */
  @define-color red        #f38ba8;
  @define-color peach      #fab387;
  @define-color yellow     #f9e2af;
  @define-color green      #a6e3a1;
  @define-color teal       #94e2d5;
  @define-color blue       #89b4fa;
  @define-color mauve      #cba6f7;
  @define-color flamingo   #f2cdcd;

  /* Neutrale Farben */
  @define-color white      #ffffff;
  @define-color black      #000000;

  /* Semantische Farben */
  @define-color shadow     @black;
  @define-color main-fg    @text;
  @define-color main-bg    @crust;
  @define-color main-br    @text;
  @define-color active-bg  @surface2;
  @define-color active-fg  @crust;
  @define-color hover-bg   @surface0;
  @define-color hover-fg   @text;

  @define-color module-fg  @text;
  @define-color workspaces @mantle;
  @define-color temperature @mantle;
  @define-color memory     @crust;
  @define-color cpu        @surface0;
  @define-color distro-fg  @black;
  @define-color distro-bg  @surface2;
  @define-color time       @surface0;
  @define-color date       @crust;
  @define-color tray       @mantle;

  @define-color pulseaudio @mantle;
  @define-color backlight  @crust;
  @define-color battery    @surface0;
  @define-color power      @surface2;

  @define-color warning    @yellow;
  @define-color critical   @red;
  @define-color charging   @green;

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
  #custom-inhibitor,
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
    padding: 0 6px;
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
    text-shadow: -3px 0 3px rgba(0, 0, 0, 0.5);
  }

  #custom-right1,
  #custom-right2,
  #custom-right3,
  #custom-right4,
  #custom-right5 {
    margin-bottom: 0;
    padding-right: 4.5px;
    text-shadow: 3px 0 3px rgba(0, 0, 0, 0.5);
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
    padding-left: 3px;
  }

  #workspaces {
    background: @workspaces;
  }

  #workspaces button {
    color: @module-fg;
    border-radius: 15px;
    box-shadow: none;
    margin: 3px 0;
    padding: 0 3px;
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
    text-shadow: 0 0 3px rgba(0, 0, 0, 0.6);
    box-shadow: 0 0 3px 1.5px rgba(0, 0, 0, 0.4);
    margin: 3px;
    padding: 0 9px;
  }

  #custom-right1 {
    color: @workspaces;
    background: @main-bg;
    text-shadow: 4.5px 0 3px rgba(0, 0, 0, 0.4);
    margin-bottom: 0;
  }

  /* ──────────────────────────────────────────────────────────┤ temperature ├─── */
  #custom-paddc {
    padding-right: 33px;
  }

  #custom-left2 {
    color: @temperature;
    background: @main-bg;
    padding-left: 4.5px;
  }

  #custom-temperature {
    background: @temperature;
    padding: 0 0 0 1.5px;
  }

  /* ───────────────────────────────────────────────────────────────┤ memory ├─── */
  #custom-left3 {
    color: @memory;
    background: @temperature;
    padding-left: 3px;
  }

  #memory {
    background: @memory;
    padding: 0 0 0 1.5px;
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
    padding-left: 4.5px;
  }

  #cpu {
    background: @cpu;
  }

  #custom-leftin1 {
    color: @cpu;
    margin-bottom: -1.5px;
  }

  /* ──────────────────────────────────────────────────────────┤ distro icon ├─── */
  #custom-left5 {
    color: @distro-bg;
    background: @main-bg;
    text-shadow: none;
    margin-bottom: -3px;
    padding-left: 4.5px;
  }

  #custom-distro {
    color: @distro-fg;
    background: @distro-bg;
    margin: 0 -1.5px -3px 0;
    padding: 0 0 0 4.5px;
    text-shadow: 0 0 1.5px rgba(0, 0, 0, 1);
  }

  #custom-right2 {
    color: @distro-bg;
    background: @main-bg;
    text-shadow: none;
    margin-bottom: -3px;
  }

  /* ─────────────────────────────────────────────────────────────────┤ time ├─── */
  #custom-rightin1 {
    color: @time;
    margin-bottom: -1.5px;
  }

  #custom-inhibitor {
    background: @time;
    padding: 0 8px 0 8px;
  }

  #custom-inhibitor:hover {
    color: @hover-fg;
  }

  #clock.time {
    background: @time;
    margin-left: -3px;
    padding: 0 4.5px 0 0;
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
    padding: 0 12px 0 7.5px;
  }

  #network:hover {
    color: @hover-fg;
  }

  /* ────────────────────────────────────────────────────────────┤ bluetooth ├─── */
  #bluetooth {
    background: @tray;
    padding-right: 7.5px;
  }

  #bluetooth:hover {
    color: @hover-fg;
  }

  /* ────────────────────────────────────────────────────────┤ system update ├─── */
  #custom-update {
    padding-right: 12px;
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
    padding: 0 12px 0;
  }

  #mpris:hover {
    color: @hover-fg;
  }

  /* ──────────────────────────────────────────────────────┤ output device ├─── */
  #custom-left6 {
    color: @pulseaudio;
    background: @main-bg;
    padding-left: 4.5px;
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
    padding-left: 3px;
  }

  #custom-nightmode {
    background: @backlight;
    padding-left: 3px
  }

  #custom-nightmode:hover {
    color: @hover-fg;
  }

  /* ──────────────────────────────────────────────────────────────┤ swaync ├─── */
  #custom-left8 {
    color: @battery;
    background: @backlight;
    padding-left: 3px;
  }

  #custom-swaync {
    background: @battery;
    color: @module-fg;
    padding-left: 3px
  }

  #custom-swaync:hover {
    color: @hover-fg;
  }

  /* ─────────────────────────────────────────────────────────┤ power button ├─── */
  #custom-leftin2 {
    color: @battery;
    background: @main-bg;
    margin-bottom: -1.5px;
  }

  #custom-power {
    color: @main-bg;
    background: @power;
    text-shadow: 0 0 3px rgba(0, 0, 0, 0.6);
    box-shadow: 0 0 3px 1.5px rgba(0, 0, 0, 0.6);
    border-radius: 15px;
    margin: 3px 6px 3px 0;
    padding: 0 9px 0 13.5px;
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
    font-size: 15px;
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
    font-size: 22.02px;
  }

  /* ─────────────────────────────────────────────────┤ left & right inverse ├─── */
  #custom-leftin1,
  #custom-leftin2,
  #custom-rightin1 {
    font-size: 23.25px;
  }

  /* ──────────────────────────────────────────────────────────┤ distro icon ├─── */
  #custom-distro {
    font-size: 21.9px;
  }

  #custom-left5,
  #custom-right2 {
    font-size: 23.52px;
  }

  /* ───────────────────────────────────────────────────────────┤ workspaces ├─── */
  #workspaces button {
    border-radius: 12px;
    padding: 0 3px;
  }

  #workspaces button.active {
    padding: 0 9px;
  }

  /* ─────────────────────────────────────────────────────────┤ power button ├─── */
  #custom-power {
    border-radius: 15px;
    padding: 0 9px 0 13.5px;
  }

''
