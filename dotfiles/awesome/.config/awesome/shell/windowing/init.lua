-- shell/windowing/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end
	return nil
end

local Policies = {
	signals = safe_require("shell.windowing.policies.signals"),
	rules = safe_require("shell.windowing.policies.rules"),
	focus = safe_require("shell.windowing.policies.focus"),
	gaps = safe_require("shell.windowing.policies.gaps"),
	minimize_stack = safe_require("shell.windowing.policies.minimize"),
	fullscreen_dim = safe_require("shell.windowing.policies.fullscreen_dim"),
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
	if Policies.rules and Policies.rules.apply then
		Policies.rules.apply({
			modkey = args.modkey or cfg.modkey,
			mouse = args.mouse or cfg.mouse,
		})
	end

	-- 3) Policies konfigurieren
	if Policies.focus and Policies.focus.init then
		Policies.focus.init({
			sloppy_focus = true,
			center_mouse_on_focus = true,
			raise_on_mouse_focus = false,
			block_ms = 150,
		})
	end
	if Policies.gaps and Policies.gaps.init then
		Policies.gaps.init({
			gap_default_dpi = 8,
			keep_gap_single_client = true,
		})
	end

	-- 4) Container (Styling)
	Container.init({ shape_fn = shape_fn })

	-- 5) Signals
	if Policies.signals and Policies.signals.apply then
		Policies.signals.apply({
			attach_titlebar = function(c)
				Titlebar.attach_titlebar(c, button_style)
			end,
			focus = Policies.focus,
			container = Container,
			minimize_stack = Policies.minimize_stack,
		})
	end

	-- 6) Fullscreen-Dim optional anschalten (per-Client-Flag Policy)
	if Policies.fullscreen_dim and Policies.fullscreen_dim.init then
		Policies.fullscreen_dim.init({
			-- dim_bg = beautiful.dim_overlay_bg, -- optional override
			-- never_dim_primary = false,
		})
	end
end

return M
