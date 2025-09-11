-- ~/.config/awesome/features/windowing/init.lua
local M = {}

function M.init(opts)
	opts = opts or {}
	local rules = require("features.windowing.rules")
	local clientSignals = require("features.windowing.client_signals")
	local taskbar = require("features.windowing.taskbar")

	rules.apply({
		modkey = opts.rules_cfg and opts.rules_cfg.modkey or opts.modkey,
		mouse = opts.mouse,
	})

	clientSignals.apply({
		sloppy_focus = opts.client_opts and opts.client_opts.sloppy_focus,
		taskbar = taskbar,
		mouse = opts.mouse,
		titlebar_opts = opts.titlebar_opts or { position = "top", size = 28 },
	})
end

return M
