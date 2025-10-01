-- ~/.config/awesome/shell/windowing/init.lua
local M = {}

function M.init(args)
	args = args or {}
	local rules = require("shell.windowing.rules")
	local clientSignals = require("shell.windowing.client_signals")

	local cfg = args.cfg or {}
	local ui = args.ui or {} -- kommt aus shell.init({ ui = ui })
	local theme_mod = ui.theme and ui.theme.windows

	-- >>> WICHTIG: Theme initialisieren, damit beautiful.* gesetzt ist
	if theme_mod and theme_mod.init then
		-- du kannst hier cfg.theme o.ä. reinreichen; notfalls cfg selbst
		pcall(theme_mod.init, cfg.theme or cfg)
	end

	local opts = {
		modkey = cfg.modkey,
		mouse = (args.input and args.input.mouse) or cfg.mouse,
		sloppy_focus = (cfg.client_opts and cfg.client_opts.sloppy_focus) ~= nil and cfg.client_opts.sloppy_focus
			or true,
		titlebar_opts = cfg.titlebar_opts or { position = "top", size = 28 },
		style = theme_mod, -- Border/Shape
		taskbar = theme_mod, -- liefert attach_titlebar
	}

	rules.apply({ modkey = opts.modkey, mouse = opts.mouse })

	clientSignals.apply({
		sloppy_focus = opts.sloppy_focus,
		taskbar = { attach = theme_mod and theme_mod.attach_titlebar }, -- wichtig
		mouse = opts.mouse,
		titlebar_opts = opts.titlebar_opts,
		style = opts.style,
	})
end

return M
