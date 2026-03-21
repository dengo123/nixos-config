-- ~/.config/awesome/shell/windowing/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	actions = nil,
	api = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function group_api(name)
	return api()[name] or {}
end

local function behavior_api()
	return group_api("behavior")
end

local function runtime_api()
	return group_api("runtime")
end

local function ui_api()
	return group_api("ui")
end

local function modules_api()
	return group_api("modules")
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

local function build_runtime_api(cfg)
	local Behavior = behavior_api()
	local Runtime = runtime_api()
	local UI = ui_api()
	local Modules = modules_api()

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

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or {}
	local windowing_cfg = cfg.windowing or {}
	local focus_cfg = windowing_cfg.focus or {}
	local fullscreen_dim_cfg = windowing_cfg.fullscreen_dim or {}

	local modkey = args.modkey
	local mouse = args.mouse

	M.api = {
		behavior = {
			focus = safe_require("shell.windowing.behavior.focus"),
			fullscreen_dim = safe_require("shell.windowing.behavior.fullscreen_dim"),
			titlebars = safe_require("shell.windowing.behavior.titlebars"),
		},
		runtime = {
			actions = safe_require("shell.windowing.runtime.actions"),
			signals = safe_require("shell.windowing.runtime.signals"),
			state = safe_require("shell.windowing.runtime.state"),
		},
		ui = {
			container = safe_require("shell.windowing.ui.container"),
			theme = safe_require("shell.windowing.ui.theme"),
			titlebar_buttons = safe_require("shell.windowing.ui.titlebar_buttons"),
		},
		modules = {
			clients = safe_require("shell.windowing.clients"),
			floating = safe_require("shell.windowing.floating"),
			rules = safe_require("shell.windowing.rules"),
		},
	}

	local Runtime = runtime_api()
	local UI = ui_api()
	local Behavior = behavior_api()

	assert(Runtime.actions and type(Runtime.actions) == "table", "windowing.init: runtime.actions fehlt")
	assert(Runtime.state and type(Runtime.state) == "table", "windowing.init: runtime.state fehlt")
	assert(UI.container and type(UI.container) == "table", "windowing.init: ui.container fehlt")
	assert(UI.theme and type(UI.theme) == "table", "windowing.init: ui.theme fehlt")

	local runtime = build_runtime_api(cfg)

	M.api.runtime_api = runtime
	M.actions = runtime.actions

	init_module(runtime.actions, {
		cfg = cfg,
		api = runtime,
	})

	init_module(runtime.state, {
		cfg = cfg,
		api = runtime,
	})

	init_module(runtime.ui.theme, {
		cfg = cfg,
		api = runtime,
	})

	local shape_fn = runtime.ui.theme and runtime.ui.theme.shape_fn and runtime.ui.theme.shape_fn() or nil
	local button_style = runtime.ui.theme and runtime.ui.theme.button_style and runtime.ui.theme.button_style(cfg) or {}

	apply_module(runtime.rules, {
		cfg = cfg,
		api = runtime,
		modkey = modkey,
		mouse = mouse,
	})

	init_module(runtime.behavior.focus, {
		cfg = cfg,
		api = runtime,
		raise_on_mouse = focus_cfg.raise_on_mouse,
		block_ms = focus_cfg.block_ms,
		center_mouse = focus_cfg.center_mouse,
	})

	init_module(runtime.titlebars, {
		cfg = cfg,
		api = runtime,
	})

	init_module(runtime.ui.container, {
		cfg = cfg,
		api = runtime,
		shape_fn = shape_fn,
		rounded_corners = (windowing_cfg.rounded_corners ~= false),
	})

	apply_module(runtime.runtime.signals, {
		cfg = cfg,
		api = runtime,
		focus = runtime.behavior.focus,
		container = runtime.ui.container,
		attach_titlebar = function(c)
			runtime.ui.container.attach_titlebar(c, button_style, runtime.actions, cfg, {
				api = runtime,
			})
		end,
	})

	if Behavior.fullscreen_dim and type(Behavior.fullscreen_dim.init) == "function" then
		local dim_cfg = fullscreen_dim_cfg

		if dim_cfg ~= false then
			if type(dim_cfg) ~= "table" then
				dim_cfg = { enabled = true }
			end

			Behavior.fullscreen_dim.init(dim_cfg)
		end
	end

	return M
end

return M
