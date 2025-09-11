-- ~/.config/awesome/features/shell/init.lua
local awful = require("awful")

local M = {
	menu = require("features.shell.menu"),
	bar = {
		model = require("features.shell.model"),
		view = require("features.shell.view"),
	},
}

-- Wrapper: Bar-Setup (Screen → Model → View)
function M.bar.setup(s, opts)
	local model = M.bar.model.build(s, opts or {})
	return M.bar.view.place(s, model, opts or {})
end

-- High-Level-Init: Menü übernehmen (aus menu.setup) + Bars pro Screen
function M.init(cfg)
	cfg = cfg or {}

	-- Menü im Menü-Modul bauen und fertige Objekte übernehmen
	local mw = M.menu.setup(cfg) -- { menu = api, launcher = widget }
	cfg.mymainmenu = mw.menu -- API mit :show/:hide/:toggle/:focus_search (falls vorhanden)
	cfg.mylauncher = mw.launcher -- fertiges Launcher-Widget

	-- globales Keyboardlayout-Widget (einmal, an alle Bars gereicht)
	local mykeyboardlayout = awful.widget.keyboardlayout()

	-- Bars pro Screen platzieren
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			launcher = cfg.mylauncher,
			keyboardlayout = mykeyboardlayout,
		})
	end)
end

return M
