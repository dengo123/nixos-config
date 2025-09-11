local config = require("system.config")
local errors = require("system.errors")

local M = {
	config = config,
	errors = errors,
}

--- Initialisiert das gesamte System:
--- - Fehler-Hooks
--- - UI (Theme + Wallpaper)
--- - Workspaces (Layouts, Wallpaper, Tags)
--- - Windowing (Rules, Client-Signals, Titlebar)
--- - Shell (Menü + Bar)
--- - Input (Keys + Root-Mouse)
--- Gibt die finale Config zurück.
--- @param overrides table|nil
function M.init(overrides)
	-- 1. Konfiguration + Fehler
	if overrides then
		for k, v in pairs(overrides) do
			M.config[k] = v
		end
	end
	if M.errors and M.errors.hook then
		M.errors.hook()
	end
	local cfg = M.config

	-- 2. UI
	local ui = require("ui")
	ui.init(cfg)

	-- 3. Workspaces
	local workspaces = require("features.workspaces")
	workspaces.init({
		wallpaper_fn = ui.wallpaper.set,
	})

	-- 4. Windowing
	local input = require("input")
	local windowing = require("features.windowing")
	windowing.init({
		modkey = cfg.modkey,
		mouse = input.mouse,
	})

	-- 5. Shell (setzt cfg.mymainmenu / cfg.mylauncher)
	local shell = require("features.shell")
	shell.init(cfg)

	-- 6. Input (nutzt cfg.mymainmenu von Shell)
	input.apply(cfg)

	return cfg
end

return M
