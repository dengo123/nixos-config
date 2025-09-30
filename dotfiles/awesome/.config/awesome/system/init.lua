-- ~/.config/awesome/system/init.lua
local config = require("system.config")
local errors = require("system.errors")

local M = {
	config = config,
	errors = errors,
}

function M.init(overrides)
	-- 1) Konfiguration + Fehler
	if overrides then
		for k, v in pairs(overrides) do
			M.config[k] = v
		end
	end
	if M.errors and M.errors.hook then
		M.errors.hook()
	end
	local cfg = M.config

	-- 2) UI (Theme + Wallpaper)
	local ui = require("ui")
	ui.init(cfg)

	-- 3) Shell (kümmert sich um Workspaces, Windowing, Menu/Bar, Notify)
	local input = require("input")
	local shell = require("shell")
	shell.init({
		cfg = cfg,
		ui = ui,
		input = input, -- für mouse + später keybindings
	})

	-- 4) Input (nutzt mymainmenu/mylauncher aus shell.init)
	input.apply(cfg)

	return cfg
end

return M
