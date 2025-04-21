{ ... }:

{
  programs.nixvim.plugins = {
    dap.enable = true;
    dap-ui.enable = true;
  };

  programs.nixvim.keymaps = [
    {
      mode = "n";
      key = "<leader>dt";
      action = ":lua require'dap'.toggle_breakpoint()<CR>";
      options.desc = "DAP: Toggle breakpoint";
    }
    {
      mode = "n";
      key = "<leader>dc";
      action = ":lua require'dap'.continue()<CR>";
      options.desc = "DAP: Continue";
    }
    {
      mode = "n";
      key = "<leader>dr";
      action = ":lua require'dap'.repl.toggle()<CR>";
      options.desc = "DAP: Toggle REPL";
    }
    {
      mode = "n";
      key = "<leader>du";
      action = ":lua require'dapui'.toggle()<CR>";
      options.desc = "DAP: Toggle UI";
    }
  ];
}

