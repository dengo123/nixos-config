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

-- High-Level-Init: Menü + Bar pro Screen
function M.init(cfg)
	-- Menü/Launcher
	local mw = M.menu.create({
		cfg = cfg,
		awesome_icon = beautiful.awesome_icon,
	})
	cfg.mylauncher = mw.launcher
	cfg.mymainmenu = mw.menu
	menubar.utils.terminal = cfg.terminal

	-- Keyboard-Layout-Widget (global, aber pro Screen übergeben)
	local mykeyboardlayout = awful.widget.keyboardlayout()

	-- pro Screen Bar
	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			launcher = cfg.mylauncher,
			keyboardlayout = mykeyboardlayout,
		})
	end)
end

return M
