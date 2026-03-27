local awful = require("awful")

local M = {}

local runtime = {
	ctx = {},
	api = {},
	cfg = {},
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return runtime.cfg or (ctx().cfg or {})
end

local function ui()
	return runtime.ui or (ctx().ui or {})
end

local function api()
	return runtime.api or {}
end

local function widgets_api()
	return (api() and api().widgets) or {}
end

local function widget(name)
	return widgets_api()[name]
end

local function resolve_menu_api(local_args)
	local c = (local_args and local_args.ctx) or ctx()

	return (local_args and local_args.menu_api)
		or (c.features and c.features.menu)
		or (c.shell and c.shell.menu)
		or (c.api and c.api.menu)
		or nil
end

local function resolve_modkey(local_args)
	local c = (local_args and local_args.ctx) or ctx()
	local conf = (local_args and local_args.cfg) or cfg()

	return local_args.modkey or c.modkey or (conf.input and conf.input.modkey) or "Mod4"
end

local function build_tabs(s, args)
	local WTabs = widget("tabs")
	local modkey = resolve_modkey(args)
	local props = args.props or {}
	local tabs_theme = args.tabs_theme or {}
	local menu_api = resolve_menu_api(args)

	if not (WTabs and type(WTabs.build) == "function") then
		return nil
	end

	return WTabs.build(s, {
		modkey = modkey,
		group_by_class = true,
		theme = tabs_theme,
		bar_height = props.height,
		menu_api = menu_api and {
			show_for_widget_with_clients_at = function(widget_, clients, anchor)
				menu_api.show_for_tabs_widget_with_clients_at(s, widget_, clients, anchor)
			end,
			is_open = function()
				return menu_api.is_open()
			end,
			hide = function()
				return menu_api.hide()
			end,
		} or nil,
	})
end

local function build_tags(s, args)
	local WTags = widget("tags")
	local conf = args.cfg or cfg()
	local show = (args.show == true)

	if not (show and WTags and type(WTags.build) == "function") then
		return nil
	end

	return WTags.build(s, {
		cfg = conf,
	})
end

local function build_tray(_s, args)
	local WSystray = widget("systray")
	local show = (args.show == true)

	if not (show and WSystray and type(WSystray.build) == "function") then
		return nil
	end

	return WSystray.build({})
end

local function build_clock(s, args)
	local WClock = widget("clock")
	local conf = args.cfg or cfg()
	local props = args.props or {}
	local bar_cfg = conf.bar or {}
	local clock_cfg = bar_cfg.clock or {}

	if not (WClock and type(WClock.build) == "function") then
		return nil
	end

	return WClock.build(s, {
		show_seconds = (clock_cfg.show_seconds == true),
		app = conf.apps and conf.apps.calendar,
		calendar_enable = (clock_cfg.calendar_enable ~= false),
		calendar_use_menu_theme = (clock_cfg.calendar_use_menu_theme == true),
		bar_position = props.position,
	})
end

local function build_layoutbox(s, _args)
	local WLayoutbox = widget("layoutbox")

	if not (WLayoutbox and type(WLayoutbox.build) == "function") then
		return nil
	end

	return WLayoutbox.build(s)
end

local function build_start(s, args)
	local WStart = widget("start")
	local conf = args.cfg or cfg()
	local props = args.props or {}
	local start_theme = args.start_theme or {}
	local show = (args.show == true)
	local start_action = args.start_action
	local menu_api = resolve_menu_api(args)

	if not (show and WStart and type(WStart.build) == "function") then
		return nil
	end

	return WStart.build({
		screen = s,
		theme = start_theme,
		bar_height = props.height,
		cfg = conf,
		start_action = start_action,
		menu_api = menu_api and {
			show_for_start_widget = function(_screen, widget_)
				menu_api.show_for_start_widget(s, widget_)
			end,
			hide = function()
				menu_api.hide()
			end,
			is_open = function()
				return menu_api.is_open()
			end,
			get_start_items = function()
				return menu_api.get_start_items()
			end,
		} or nil,
	})
end

local function build_notify(s, args)
	local WNotify = widget("notify")
	local show = (args.show == true)

	if not (show and WNotify and type(WNotify.build) == "function") then
		return nil
	end

	return WNotify.build(s, {})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.ctx = args.ctx or args or {}
	runtime.api = args.api or {}
	runtime.cfg = args.cfg or (runtime.ctx.cfg or {})
	runtime.ui = args.ui or (runtime.ctx.ui or {})

	return M
end

function M.build(s, args)
	args = args or {}

	local c = args.ctx or ctx()
	local conf = args.cfg or cfg()

	return {
		tabs = build_tabs(s, {
			ctx = c,
			cfg = conf,
			modkey = args.modkey,
			props = args.props,
			tabs_theme = args.tabs_theme,
			menu_api = args.menu_api,
		}),
		tags = build_tags(s, {
			ctx = c,
			cfg = conf,
			show = (args.show_tags == true),
		}),
		tray = build_tray(s, {
			ctx = c,
			show = (args.showtray == true),
		}),
		clock = build_clock(s, {
			ctx = c,
			cfg = conf,
			props = args.props,
		}),
		layoutbox = build_layoutbox(s, {
			ctx = c,
		}),
		start_btn = build_start(s, {
			ctx = c,
			cfg = conf,
			props = args.props,
			start_theme = args.start_theme,
			show = (args.show_start == true),
			start_action = args.start_action,
			menu_api = args.menu_api,
		}),
		notify = build_notify(s, {
			ctx = c,
			show = (args.show_notify == true),
		}),
	}
end

return M
