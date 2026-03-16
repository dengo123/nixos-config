-- ~/.config/awesome/shell/windowing/init.lua
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
	local system_cfg = cfg.system or {}
	local focus_cfg = cfg.focus or {}

	-- 1) Theme laden (setzt beautiful.*)
	if theme and theme.init then
		pcall(theme.init, cfg)
	end

	local shape_fn = theme and theme.shape_fn and theme.shape_fn() or nil
	local button_style = theme and theme.button_style and theme.button_style(cfg) or {}

	-- 2) Regeln
	if Policies.rules and Policies.rules.apply then
		Policies.rules.apply({
			modkey = args.modkey or system_cfg.modkey or cfg.modkey,
			mouse = args.mouse,
			cfg = cfg,
		})
	end

	-- 3) Fokus-Policy
	if Policies.focus and Policies.focus.init then
		Policies.focus.init(focus_cfg)
	end

	-- 4) Container (Styling)
	Container.init({
		shape_fn = shape_fn,
	})

	-- 5) Client-Signale
	if Policies.signals and Policies.signals.apply then
		Policies.signals.apply({
			attach_titlebar = function(c)
				Titlebar.attach_titlebar(c, button_style)
			end,
			focus = Policies.focus,
			container = Container,
		})
	end

	-- 6) Fullscreen-Dim optional
	if Policies.fullscreen_dim and Policies.fullscreen_dim.init then
		Policies.fullscreen_dim.init({})
	end
end

return M
