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

local runtime = {
	ctx = {},
	windowing = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function ensure_ctx_roots()
	local c = ctx()

	c.shell = c.shell or {}
	c.features = c.features or {}
	c.api = c.api or {}
	c.external = c.external or {}
	c.cfg = c.cfg or {}
	c.cfg.api = c.cfg.api or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function api()
	return M.api or {}
end

local function group_api(name)
	return api()[name] or {}
end

local function behavior_api()
	return group_api("behavior")
end

local function policy_api()
	return group_api("policy")
end

local function runtime_api()
	return group_api("runtime")
end

local function ui_api()
	return group_api("ui")
end

local function init_module(mod, args)
	if mod and type(mod.init) == "function" then
		return mod.init(args)
	end

	return mod
end

local function apply_module(mod, args)
	if mod and type(mod.apply) == "function" then
		return mod.apply(args)
	end

	return mod
end

local function build_windowing()
	local c = ctx()
	local conf = cfg()
	local windowing_cfg = conf.windowing or {}

	local Behavior = behavior_api()
	local Policy = policy_api()
	local RuntimeMods = runtime_api()
	local UI = ui_api()

	return {
		ctx = c,
		cfg = conf,
		ui = ui(),
		input = c.input or {},
		modkey = c.modkey,
		mouse = c.mouse,

		windowing_cfg = windowing_cfg,
		focus_cfg = windowing_cfg.focus or {},
		fullscreen_dim_cfg = windowing_cfg.fullscreen_dim or {},

		behavior = Behavior,
		policy = Policy,
		runtime = RuntimeMods,
		ui_mods = UI,

		focus = Behavior.focus,
		fullscreen_dim = Behavior.fullscreen_dim,
		titlebars = Behavior.titlebars,

		clients = Policy.clients,
		floating = Policy.floating,
		rules = Policy.rules,

		actions = RuntimeMods.actions,
		minimized = RuntimeMods.minimized,
		navigation = RuntimeMods.navigation,
		signals = RuntimeMods.signals,
		state = RuntimeMods.state,

		container = UI.container,
		theme = UI.theme,
		titlebar_buttons = UI.titlebar_buttons,

		max_policy = (c.policy and c.policy.max_policy)
			or (c.workspaces and c.workspaces.policy and c.workspaces.policy.max_policy)
			or nil,
	}
end

local function build_actions(windowing)
	return {
		scr_in_dir = windowing.actions and windowing.actions.scr_in_dir or nil,
		move_client_dir = windowing.actions and windowing.actions.move_client_dir or nil,
		move_client_to_screen = windowing.actions and windowing.actions.move_client_to_screen or nil,
		layout_state_mode = windowing.actions and windowing.actions.layout_state_mode or nil,
		is_layout_state_active = windowing.actions and windowing.actions.is_layout_state_active or nil,
		toggle_layout_state = windowing.actions and windowing.actions.toggle_layout_state or nil,

		focus_client = windowing.navigation and windowing.navigation.focus_client or nil,
		swap_client = windowing.navigation and windowing.navigation.swap_client or nil,
		is_max_layout = windowing.navigation and windowing.navigation.is_max_layout or nil,
		current_screen = windowing.navigation and windowing.navigation.current_screen or nil,

		minimize_focused = windowing.actions and windowing.actions.minimize_focused or nil,
		minimize_visible_tag_on_screen = windowing.actions and windowing.actions.minimize_visible_tag_on_screen or nil,
		minimize_all_tags_on_screen = windowing.actions and windowing.actions.minimize_all_tags_on_screen or nil,
		minimize_visible_tags_on_all_screens = windowing.actions
				and windowing.actions.minimize_visible_tags_on_all_screens
			or nil,
		minimize_all_tags_on_all_screens = windowing.actions and windowing.actions.minimize_all_tags_on_all_screens
			or nil,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	ensure_ctx_roots()

	local c = ctx()
	local conf = cfg()

	M.api = {
		behavior = {
			focus = safe_require("shell.windowing.behavior.focus"),
			fullscreen_dim = safe_require("shell.windowing.behavior.fullscreen_dim"),
			titlebars = safe_require("shell.windowing.behavior.titlebars"),
		},
		policy = {
			clients = safe_require("shell.windowing.policy.clients"),
			floating = safe_require("shell.windowing.policy.floating"),
			rules = safe_require("shell.windowing.policy.rules"),
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
	}

	c.shell.windowing = M
	c.features.windowing = M
	c.api.windowing = M
	c.external.windowing = M
	c.cfg.api.windowing = M

	runtime.windowing = build_windowing()
	M.api.windowing = runtime.windowing

	local windowing = runtime.windowing

	-- ---------------------------------------------------------------------
	-- Policy
	-- ---------------------------------------------------------------------

	init_module(windowing.floating, {
		clients = windowing.clients,
	})

	apply_module(windowing.rules, {
		cfg = conf,
		modkey = windowing.modkey,
		input = windowing.input,
		clients = windowing.clients,
		floating = windowing.floating,
		titlebars = windowing.titlebars,
	})

	-- ---------------------------------------------------------------------
	-- Runtime
	-- ---------------------------------------------------------------------

	init_module(windowing.state, {
		cfg = conf,
	})

	init_module(windowing.actions, {
		windowing = windowing,
	})

	init_module(windowing.minimized, {})
	if windowing.minimized and type(windowing.minimized.attach_signals) == "function" then
		windowing.minimized.attach_signals()
	end

	init_module(windowing.navigation, {
		windowing = windowing,
	})

	-- ---------------------------------------------------------------------
	-- UI
	-- ---------------------------------------------------------------------

	init_module(windowing.theme, {
		ui = windowing.ui,
	})

	local shape_fn = windowing.theme and windowing.theme.shape_fn and windowing.theme.shape_fn() or nil
	local button_style = windowing.theme
			and windowing.theme.button_style
			and windowing.theme.button_style({
				ui = windowing.ui,
			})
		or {}

	init_module(windowing.container, {
		shape_fn = shape_fn,
		rounded_corners = (windowing.windowing_cfg.rounded_corners ~= false),
	})

	-- ---------------------------------------------------------------------
	-- Behavior
	-- ---------------------------------------------------------------------

	init_module(windowing.focus, {
		cfg = conf,
		raise_on_mouse = windowing.focus_cfg.raise_on_mouse,
		block_ms = windowing.focus_cfg.block_ms,
		center_mouse = windowing.focus_cfg.center_mouse,
	})

	init_module(windowing.titlebars, {
		cfg = conf,
		clients = windowing.clients,
	})

	-- ---------------------------------------------------------------------
	-- Signals
	-- ---------------------------------------------------------------------

	windowing.attach_titlebar = function(client_)
		if windowing.container and type(windowing.container.attach_titlebar) == "function" then
			windowing.container.attach_titlebar(client_, button_style, windowing.actions, conf, {
				titlebar_buttons = windowing.titlebar_buttons,
				input = windowing.input,
			})
		end
	end

	init_module(windowing.signals, {
		windowing = windowing,
	})

	apply_module(windowing.signals, {
		windowing = windowing,
	})

	-- ---------------------------------------------------------------------
	-- Optional behavior
	-- ---------------------------------------------------------------------

	if windowing.fullscreen_dim and type(windowing.fullscreen_dim.init) == "function" then
		local dim_cfg = windowing.fullscreen_dim_cfg

		if dim_cfg ~= false then
			if type(dim_cfg) ~= "table" then
				dim_cfg = { enabled = true }
			end

			windowing.fullscreen_dim.init(dim_cfg)
		end
	end

	M.actions = build_actions(windowing)

	return M
end

return M
