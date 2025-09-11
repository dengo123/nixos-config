-- features/shell/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local menubar = require("menubar")

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

-- High-Level-Init: nur noch Zusammensetzen
function M.init(cfg)
	cfg = cfg or {}

	-- Menü vollständig im Menü-Modul aufbauen
	local mw = M.menu.setup(cfg)
	cfg.mylauncher = mw.launcher
	cfg.mymainmenu = mw.menu

	-- menubar Terminal (gehört eher zur App-weiten Config)
	menubar.utils.terminal = cfg.terminal

	-- globales Keyboardlayout-Widget, pro Screen gereicht
	local mykeyboardlayout = awful.widget.keyboardlayout()

	-- pro Screen Bar platzieren
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			launcher = cfg.mylauncher,
			keyboardlayout = mykeyboardlayout,
			awesome_icon = beautiful.awesome_icon,
		})
	end)
end

return M
