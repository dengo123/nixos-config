-- ~/.config/awesome/shell/windowing/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local Behavior = {
	signals = safe_require("shell.windowing.behavior.signals"),
	rules = safe_require("shell.windowing.behavior.rules"),
	focus = safe_require("shell.windowing.behavior.focus"),
	fullscreen_dim = safe_require("shell.windowing.behavior.fullscreen_dim"),
}

local Container = require("shell.windowing.ui.container")
local Theme = require("shell.windowing.ui.theme")
local Titlebar = require("shell.windowing.ui.titlebar")

local M = {
	actions = require("shell.windowing.runtime.actions"),
}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local cfg = args.cfg or {}

	local windowing_cfg = cfg.windowing or {}
	local focus_cfg = windowing_cfg.focus or {}
	local fullscreen_cfg = windowing_cfg.fullscreen or {}

	local modkey = args.modkey
	local mouse = args.mouse

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	pcall(Theme.init, cfg)

	local shape_fn = Theme.shape_fn and Theme.shape_fn() or nil
	local button_style = Theme.button_style and Theme.button_style(cfg) or {}

	-- ---------------------------------------------------------------------
	-- Rules
	-- ---------------------------------------------------------------------

	if Behavior.rules and Behavior.rules.apply then
		Behavior.rules.apply({
			modkey = modkey,
			mouse = mouse,
			cfg = cfg,
		})
	end

	-- ---------------------------------------------------------------------
	-- Focus
	-- ---------------------------------------------------------------------

	if Behavior.focus and Behavior.focus.init then
		Behavior.focus.init(focus_cfg)
	end

	-- ---------------------------------------------------------------------
	-- Container
	-- ---------------------------------------------------------------------

	Container.init({
		shape_fn = shape_fn,
		rounded_corners = (windowing_cfg.rounded_corners ~= false),
	})

	-- ---------------------------------------------------------------------
	-- Signals
	-- ---------------------------------------------------------------------

	if Behavior.signals and Behavior.signals.apply then
		Behavior.signals.apply({
			attach_titlebar = function(c)
				Titlebar.attach_titlebar(c, button_style)
			end,
			focus = Behavior.focus,
			container = Container,
		})
	end

	-- ---------------------------------------------------------------------
	-- Fullscreen Dim
	-- ---------------------------------------------------------------------

	if Behavior.fullscreen_dim and Behavior.fullscreen_dim.init then
		local dim_cfg = fullscreen_cfg.dim

		if dim_cfg ~= false then
			if type(dim_cfg) ~= "table" then
				dim_cfg = { enabled = true }
			end

			Behavior.fullscreen_dim.init(dim_cfg)
		end
	end
end

return M
