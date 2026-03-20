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
	signals = safe_require("shell.windowing.runtime.signals"),
	state = require("shell.windowing.runtime.state"),
}

local UI = {
	container = require("shell.windowing.ui.container"),
	theme = require("shell.windowing.ui.theme"),
	titlebar_buttons = require("shell.windowing.ui.titlebar_buttons"),
}

local Modules = {
	clients = require("shell.windowing.clients"),
	floating = require("shell.windowing.floating"),
	rules = safe_require("shell.windowing.rules"),
}

local M = {
	actions = nil,
	api = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function build_api(cfg)
	return {
		cfg = cfg,

		behavior = Behavior,
		runtime = Runtime,
		ui = UI,

		clients = Modules.clients,
		floating = Modules.floating,
		rules = Modules.rules,

		actions = Runtime.actions,
		state = Runtime.state,
		titlebars = Behavior.titlebars,
		titlebar_buttons = UI.titlebar_buttons,
	}
end

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		mod.init(args)
	end
end

local function apply_module(mod, args)
	if mod and type(mod.apply) == "function" then
		mod.apply(args)
	end
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

	local api = build_api(cfg)

	M.api = api
	M.actions = api.actions

	-- ---------------------------------------------------------------------
	-- Runtime
	-- ---------------------------------------------------------------------

	init_module(api.actions, {
		cfg = cfg,
		api = api,
	})

	init_module(api.state, {
		cfg = cfg,
		api = api,
	})

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	init_module(api.ui.theme, {
		cfg = cfg,
		api = api,
	})

	local shape_fn = api.ui.theme and api.ui.theme.shape_fn and api.ui.theme.shape_fn() or nil
	local button_style = api.ui.theme and api.ui.theme.button_style and api.ui.theme.button_style(cfg) or {}

	-- ---------------------------------------------------------------------
	-- Rules
	-- ---------------------------------------------------------------------

	apply_module(api.rules, {
		cfg = cfg,
		api = api,
		modkey = modkey,
		mouse = mouse,
	})

	-- ---------------------------------------------------------------------
	-- Focus
	-- ---------------------------------------------------------------------

	init_module(api.behavior.focus, {
		cfg = cfg,
		api = api,
		raise_on_mouse = focus_cfg.raise_on_mouse,
		block_ms = focus_cfg.block_ms,
		center_mouse = focus_cfg.center_mouse,
	})

	-- ---------------------------------------------------------------------
	-- Titlebars
	-- ---------------------------------------------------------------------

	init_module(api.titlebars, {
		cfg = cfg,
		api = api,
	})

	-- ---------------------------------------------------------------------
	-- Container
	-- ---------------------------------------------------------------------

	init_module(api.ui.container, {
		cfg = cfg,
		api = api,
		shape_fn = shape_fn,
		rounded_corners = (windowing_cfg.rounded_corners ~= false),
	})

	-- ---------------------------------------------------------------------
	-- Runtime Signals
	-- ---------------------------------------------------------------------

	apply_module(api.runtime.signals, {
		cfg = cfg,
		api = api,
		focus = api.behavior.focus,
		container = api.ui.container,
		attach_titlebar = function(c)
			api.ui.container.attach_titlebar(c, button_style, api.actions, cfg, {
				api = api,
			})
		end,
	})

	-- ---------------------------------------------------------------------
	-- Fullscreen Dim
	-- ---------------------------------------------------------------------

	if api.behavior.fullscreen_dim and type(api.behavior.fullscreen_dim.init) == "function" then
		local dim_cfg = fullscreen_cfg.dim

		if dim_cfg ~= false then
			if type(dim_cfg) ~= "table" then
				dim_cfg = { enabled = true }
			end

			api.behavior.fullscreen_dim.init(dim_cfg)
		end
	end

	-- ---------------------------------------------------------------------
	-- Fullscreen Portrait
	-- ---------------------------------------------------------------------

	init_module(api.behavior.fullscreen_portrait, {
		cfg = cfg,
		api = api,
	})

	return M
end

return M
