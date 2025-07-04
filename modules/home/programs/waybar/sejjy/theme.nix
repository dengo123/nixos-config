{
  config,
  lib,
  ...
}: let
  inherit
    (config.stylix.base16)
    base00
    base01
    base02
    base03
    base04
    base05
    base06
    base07
    base08
    base09
    base0A
    base0B
    base0C
    base0D
    base0E
    base0F
    ;
in {
  colors = {
    # Catppuccin Mocha Palette
    crust = base00;
    mantle = base01;
    surface0 = base02;
    surface1 = base03;
    surface2 = base04;
    text = base05;
    rosewater = base06;
    lavender = base07;

    red = base08;
    peach = base09;
    yellow = base0A;
    green = base0B;
    teal = base0C;
    blue = base0D;
    mauve = base0E;
    flamingo = base0F;

    white = "#ffffff";
    black = "#000000";

    # Semantic Mapping
    shadow = "${lib.strings.mkShade base00 0.5}";
    main-fg = base05;
    main-bg = base00;
    main-br = base05;
    active-bg = base04;
    active-fg = base00;
    hover-bg = base02;
    hover-fg = base05;

    module-fg = base05;
    workspaces = base01;
    temperature = base01;
    memory = base00;
    cpu = base02;
    distro-fg = "#000000";
    distro-bg = base04;
    time = base02;
    date = base00;
    tray = base01;

    pulseaudio = base01;
    backlight = base00;
    battery = base02;
    power = base04;

    warning = base0A;
    critical = base08;
    charging = base0B;
  };
}
