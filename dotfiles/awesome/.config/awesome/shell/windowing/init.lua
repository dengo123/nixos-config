-- shell/windowing/init.lua
local Policies = {
	signals = require("shell.windowing.policies.signals"),
	rules = require("shell.windowing.policies.rules"),
	focus = require("shell.windowing.policies.focus"),
	gaps = require("shell.windowing.policies.gaps"),
	minimize_stack = require("shell.windowing.policies.minimize"),
	fullscreen_dim = require("shell.windowing.policies.fullscreen"),
}
local Container = require("shell.windowing.container")
local Titlebar = require("shell.windowing.titlebar")

local M = {}

function M.init(args)
	args = args or {}
	local cfg = args.cfg or {}
	local ui = args.ui or {}
	local theme = ui.theme and ui.theme.windows

	-- 1) Theme laden (setzt beautiful.*)
	if theme and theme.init then
		pcall(theme.init, cfg.theme or cfg)
	end
	local shape_fn = theme and theme.shape_fn and theme.shape_fn() or nil
	local button_style = theme and theme.button_style and theme.button_style(cfg) or {}

	-- 2) Regeln
	Policies.rules.apply({
		modkey = args.modkey or cfg.modkey,
		mouse = args.mouse or cfg.mouse,
	})

	-- 3) Policies konfigurieren
	Policies.focus.init({
		sloppy_focus = true,
		center_mouse_on_focus = true,
		raise_on_mouse_focus = false,
		block_ms = 150,
	})
	Policies.gaps.init({
		gap_default_dpi = 8,
		keep_gap_single_client = true,
	})

	-- 4) Container (Styling) vorbereiten
	Container.init({ shape_fn = shape_fn })

	-- 5) Signals verkabeln
	Policies.signals.apply({
		attach_titlebar = function(c)
			Titlebar.attach_titlebar(c, button_style)
		end,
		focus = Policies.focus,
		container = Container,
		minimize_stack = Policies.minimize_stack,
	})

	-- 6) Feature anschalten
	if Policies.fullscreen_dim and Policies.fullscreen_dim.init then
		Policies.fullscreen_dim.init()
	end
end

return M
