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

local function build_runtime_api(cfg, global_ui)
	local Behavior = behavior_api()
	local Runtime = runtime_api()
	local UI = ui_api()
	local Modules = modules_api()

	return {
		cfg = cfg,
		ui_root = global_ui or {},

		behavior = Behavior,
		runtime = Runtime,
		ui = UI,

		clients = Modules.clients,
		floating = Modules.floating,
		rules = Modules.rules,

		actions = Runtime.actions,
		minimized = Runtime.minimized,
		navigation = Runtime.navigation,
		signals = Runtime.signals,
		state = Runtime.state,

		titlebars = Behavior.titlebars,
		titlebar_buttons = UI.titlebar_buttons,
	}
end

local function build_actions(runtime)
	return {
		scr_in_dir = runtime.actions and runtime.actions.scr_in_dir or nil,
		move_client_dir = runtime.actions and runtime.actions.move_client_dir or nil,
		move_client_to_screen = runtime.actions and runtime.actions.move_client_to_screen or nil,
		layout_state_mode = runtime.actions and runtime.actions.layout_state_mode or nil,
		is_layout_state_active = runtime.actions and runtime.actions.is_layout_state_active or nil,
		toggle_layout_state = runtime.actions and runtime.actions.toggle_layout_state or nil,

		focus_client = runtime.navigation and runtime.navigation.focus_client or nil,
		swap_client = runtime.navigation and runtime.navigation.swap_client or nil,
		is_max_layout = runtime.navigation and runtime.navigation.is_max_layout or nil,
		current_screen = runtime.navigation and runtime.navigation.current_screen or nil,

		minimize_focused = runtime.actions and runtime.actions.minimize_focused or nil,
		minimize_visible_tag_on_screen = runtime.actions and runtime.actions.minimize_visible_tag_on_screen or nil,
		minimize_all_tags_on_screen = runtime.actions and runtime.actions.minimize_all_tags_on_screen or nil,
		minimize_visible_tags_on_all_screens = runtime.actions and runtime.actions.minimize_visible_tags_on_all_screens
			or nil,
		minimize_all_tags_on_all_screens = runtime.actions and runtime.actions.minimize_all_tags_on_all_screens or nil,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or {}
	local global_ui = args.ui or {}

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
			minimized = safe_require("shell.windowing.runtime.minimized"),
			navigation = safe_require("shell.windowing.runtime.navigation"),
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

	local runtime = build_runtime_api(cfg, global_ui)

	M.api.runtime_api = runtime

	init_module(runtime.actions, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
	})

	init_module(runtime.state, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
	})

	init_module(runtime.minimized, {
		cfg = cfg,
		api = runtime.runtime,
		ui = runtime.ui_root,
	})

	init_module(runtime.navigation, args)

	if runtime.minimized and type(runtime.minimized.attach_signals) == "function" then
		runtime.minimized.attach_signals()
	end

	init_module(runtime.ui.theme, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
	})

	local shape_fn = runtime.ui.theme and runtime.ui.theme.shape_fn and runtime.ui.theme.shape_fn() or nil
	local button_style = runtime.ui.theme
			and runtime.ui.theme.button_style
			and runtime.ui.theme.button_style({
				cfg = cfg,
				api = runtime,
				ui = runtime.ui_root,
			})
		or {}

	apply_module(runtime.rules, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
		modkey = modkey,
		mouse = mouse,
	})

	init_module(runtime.behavior.focus, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
		raise_on_mouse = focus_cfg.raise_on_mouse,
		block_ms = focus_cfg.block_ms,
		center_mouse = focus_cfg.center_mouse,
	})

	init_module(runtime.titlebars, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
	})

	init_module(runtime.ui.container, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
		shape_fn = shape_fn,
		rounded_corners = (windowing_cfg.rounded_corners ~= false),
	})

	apply_module(runtime.runtime.signals, {
		cfg = cfg,
		api = runtime,
		ui = runtime.ui_root,
		focus = runtime.behavior.focus,
		container = runtime.ui.container,
		attach_titlebar = function(c)
			runtime.ui.container.attach_titlebar(c, button_style, runtime.actions, cfg, {
				api = runtime,
				ui = runtime.ui_root,
			})
		end,
	})

	if runtime.behavior.fullscreen_dim and type(runtime.behavior.fullscreen_dim.init) == "function" then
		local dim_cfg = fullscreen_dim_cfg

		if dim_cfg ~= false then
			if type(dim_cfg) ~= "table" then
				dim_cfg = { enabled = true }
			end

			runtime.behavior.fullscreen_dim.init(dim_cfg)
		end
	end

	M.actions = build_actions(runtime)

	return M
end

return M
