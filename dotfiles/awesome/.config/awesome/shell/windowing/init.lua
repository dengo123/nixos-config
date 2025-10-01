-- ~/.config/awesome/shell/windowing/init.lua
local M = {}

function M.init(args)
	args = args or {}

	-- lazy requires (halten reloads schnell)
	local rules = require("shell.windowing.rules")
	local clientSignals = require("shell.windowing.client_signals")
	local fullscreenDim = require("shell.windowing.fullscreen_dim") -- << neu

	-- Eingaben normalisieren
	local cfg = args.cfg or {}
	local ui = args.ui or {} -- kommt aus shell.init({ ui = ui })
	local inp = args.input or {}

	-- modkey/mouse aus args **oder** cfg lesen (robust gegen ältere Aufrufer)
	local modkey = args.modkey or cfg.modkey
	local mouse = args.mouse or inp.mouse or cfg.mouse

	-- Theme-Modul (für Fensterrahmen/Titlebars etc.)
	local theme_mod = ui.theme and ui.theme.windows

	-- Theme früh initialisieren, damit beautiful.* sicher gesetzt ist
	if theme_mod and theme_mod.init then
		pcall(theme_mod.init, cfg.theme or cfg)
	end

	local opts = {
		modkey = modkey,
		mouse = mouse,
		sloppy_focus = (cfg.client_opts and cfg.client_opts.sloppy_focus) ~= nil and cfg.client_opts.sloppy_focus
			or true,
		titlebar_opts = cfg.titlebar_opts or { position = "top", size = 28 },
		style = theme_mod, -- Border/Shape/Colors
		taskbar = theme_mod, -- liefert attach_titlebar, falls vorhanden
	}

	-- 1) Regeln (Floating, Dialoge, Portrait-Sizing usw.)
	rules.apply({ modkey = opts.modkey, mouse = opts.mouse })

	-- 2) Client-Signale (Titlebars, Focus, Minimize-Stack, Styles …)
	clientSignals.apply({
		sloppy_focus = opts.sloppy_focus,
		taskbar = { attach = theme_mod and theme_mod.attach_titlebar },
		mouse = opts.mouse,
		titlebar_opts = opts.titlebar_opts,
		style = opts.style,
	})

	-- 3) Fullscreen-Dimming der **anderen** Monitore aktivieren
	--    (Screen mit FS bleibt hell; konfigurierbar in fullscreen_dim.lua)
	fullscreenDim.init()
end

return M
