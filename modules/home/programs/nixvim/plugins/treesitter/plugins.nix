{ ... }:

{
  programs.nixvim.config.plugins.treesitter-textobjects = {
    enable = true;

    select = {
      enable = true;
      lookahead = true;
      keymaps = {
        "af" = "@function.outer";
        "if" = "@function.inner";
        "ac" = "@class.outer";
        "ic" = "@class.inner";
        "ap" = "@parameter.outer";
        "ip" = "@parameter.inner";
        "ao" = "@conditional.outer";
        "io" = "@conditional.inner";
        "al" = "@loop.outer";
        "il" = "@loop.inner";
        "aa" = "@call.outer";
        "ia" = "@call.inner";
        "ab" = "@block.outer";
        "ib" = "@block.inner";
        "as" = "@statement.outer";
        "is" = "@statement.inner";
        "a/" = "@comment.outer";
        "i/" = "@comment.inner";
      };
    };

    move = {
      enable = true;
      setJumps = true;
      gotoNextStart = {
        "]f" = "@function.outer";
        "]c" = "@class.outer";
        "]l" = "@loop.outer";
        "]o" = "@conditional.outer";
        "]a" = "@parameter.outer";
      };
      gotoPreviousStart = {
        "[f" = "@function.outer";
        "[c" = "@class.outer";
        "[l" = "@loop.outer";
        "[o" = "@conditional.outer";
        "[a" = "@parameter.outer";
      };
    };

    swap = {
      enable = true;
      swapNext = {
        "<leader>ta" = "@parameter.inner";
        "<leader>tc" = "@call.inner";
      };
      swapPrevious = {
        "<leader>tA" = "@parameter.inner";
        "<leader>tC" = "@call.inner";
      };
    };

    lspInterop = {
      enable = true;
      border = "single";
      peekDefinitionCode = {
        "<leader>tp" = "@function.outer";
        "<leader>tc" = "@class.outer";
        "<leader>tl" = "@loop.outer";
      };
    };
  };
}

