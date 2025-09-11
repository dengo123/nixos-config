-- ~/.config/awesome/features/shell/init.lua
local awful = require("awful")

local M = {
	menu = require("features.shell.menu"),
	bar = {
		model = require("features.shell.model"),
		view = require("features.shell.view"),
	},
}

function M.bar.setup(s, opts)
	local model = M.bar.model.build(s, opts or {})
	return M.bar.view.place(s, model, opts or {})
end

function M.init(cfg)
	cfg = cfg or {}

	-- Menü an cfg andocken (setzt cfg.mymainmenu / cfg.mylauncher)
	M.menu.attach(cfg)

	-- globales Keyboardlayout-Widget erstellen und an Bars reichen
	local mykeyboardlayout = awful.widget.keyboardlayout()

	awful.screen.connect_for_each_screen(function(s)
		M.bar.setup(s, {
			cfg = cfg,
			launcher = cfg.mylauncher, -- kommt aus menu.attach()
			keyboardlayout = mykeyboardlayout,
		})
	end)
end

return M
