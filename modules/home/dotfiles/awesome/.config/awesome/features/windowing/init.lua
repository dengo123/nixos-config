-- ~/.config/awesome/features/windowing/init.lua
local M = {}

function M.init(cfg)
	cfg = cfg or {}

	local rules = require("features.windowing.rules")
	local clientSignals = require("features.windowing.client_signals")
	local taskbar = require("features.windowing.taskbar")

	-- Defaults -------------------------------------------------------------
	local opts = {
		modkey = cfg.modkey,
		mouse = cfg.mouse,

		-- sloppy_focus Default: wenn cfg.client_opts.sloppy_focus gesetzt ist, dann nutzen,
		-- sonst true als Fallback
		sloppy_focus = (cfg.client_opts and cfg.client_opts.sloppy_focus) ~= nil and cfg.client_opts.sloppy_focus
			or true,

		-- Titlebar-Optionen
		titlebar_opts = cfg.titlebar_opts or { position = "top", size = 28 },
	}

	-- Apply rules ----------------------------------------------------------
	rules.apply({
		modkey = opts.modkey,
		mouse = opts.mouse,
	})

	-- Apply client signals -------------------------------------------------
	clientSignals.apply({
		sloppy_focus = opts.sloppy_focus,
		taskbar = taskbar,
		mouse = opts.mouse,
		titlebar_opts = opts.titlebar_opts,
	})
end

return M
