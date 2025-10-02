-- ~/.config/awesome/shell/menu/lib/items.lua
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Du kannst auch `return function(ctx) ... end` zurückgeben.
-- Hier nutze ich ein Modul mit .items(ctx), was Lib.defaults unterstützt.
local M = {}

function M.items(ctx)
	local cfg = ctx and ctx.cfg or {}
	local dialogs = ctx and ctx.dialogs or {}

	local launcher_cmd = (type(cfg.launcher) == "string" and #cfg.launcher > 0) and cfg.launcher or "rofi -show drun"
	local files_cmd = (cfg.files_cmd and #cfg.files_cmd > 0) and cfg.files_cmd or "nemo"

	return {
		{
			"files",
			function()
				awful.spawn.with_shell(files_cmd)
			end,
		},
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{
			"launcher",
			function()
				if dialogs.launcher then
					dialogs.launcher()
				else
					awful.spawn.with_shell(launcher_cmd)
				end
			end,
		},
		{
			"power",
			function()
				if dialogs.power then
					dialogs.power()
				end
			end,
		},
	}
end

return M
