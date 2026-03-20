-- ~/.config/awesome/shell/windowing/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local Behavior = {
	focus = safe_require("shell.windowing.behavior.focus"),
	fullscreen_dim = safe_require("shell.windowing.behavior.fullscreen_dim"),
	titlebars = safe_require("shell.windowing.behavior.titlebars"),
}

local Runtime = {
	actions = require("shell.windowing.runtime.actions"),
	state = require("shell.windowing.runtime.state"),
	signals = safe_require("shell.windowing.runtime.signals"),
}

local UI = {
	container = require("shell.windowing.ui.container"),
	theme = require("shell.windowing.ui.theme"),
	titlebar = require("shell.windowing.ui.titlebar"),
}

local Modules = {
	clients = require("shell.windowing.clients"),
	floating = require("shell.windowing.floating"),
	rules = safe_require("shell.windowing.rules"),
}

local M = {
	actions = Runtime.actions,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function build_api()
	return {
		clients = Modules.clients,
		floating = Modules.floating,
		actions = Runtime.actions,
		state = Runtime.state,
		titlebars = Behavior.titlebars,
	}
end

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

	local api = build_api()

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	UI.theme.init(cfg)

	local shape_fn = UI.theme.shape_fn and UI.theme.shape_fn() or nil
	local button_style = UI.theme.button_style and UI.theme.button_style(cfg) or {}

	-- ---------------------------------------------------------------------
	-- Rules
	-- ---------------------------------------------------------------------

	if Modules.rules and Modules.rules.apply then
		Modules.rules.apply({
			modkey = modkey,
			mouse = mouse,
			cfg = cfg,
			api = api,
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

	UI.container.init({
		shape_fn = shape_fn,
		rounded_corners = (windowing_cfg.rounded_corners ~= false),
	})

	-- ---------------------------------------------------------------------
	-- Signals
	-- ---------------------------------------------------------------------

	if Runtime.signals and Runtime.signals.apply then
		Runtime.signals.apply({
			cfg = cfg,
			api = api,
			attach_titlebar = function(c)
				UI.titlebar.attach_titlebar(c, button_style, Runtime.actions, cfg)
			end,
			focus = Behavior.focus,
			container = UI.container,
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
