{
  stylix,
  lib,
  ...
}: {
  colors = let
    inherit
      (stylix)
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
    # Catppuccin Mocha Basis
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

    # Semantik
    shadow = "#${base00}80";
    main-fg = text;
    main-bg = crust;
    main-br = text;
    active-bg = surface2;
    active-fg = crust;
    hover-bg = surface0;
    hover-fg = text;

    module-fg = text;
    workspaces = mantle;
    temperature = mantle;
    memory = base;
    cpu = surface0;
    distro-fg = black;
    distro-bg = surface2;
    time = surface0;
    date = base;
    tray = mantle;

    pulseaudio = mantle;
    backlight = base;
    battery = surface0;
    power = surface2;

    warning = yellow;
    critical = red;
    charging = green;
  };
}
