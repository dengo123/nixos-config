{ theme, ... }:

{
  highlightGroups = {
    Normal = { fg = theme.base05; bg = "NONE"; };
    LineNr = { fg = theme.base03; };
    CursorLineNr = { fg = theme.base08; bold = true; };
  };
}

