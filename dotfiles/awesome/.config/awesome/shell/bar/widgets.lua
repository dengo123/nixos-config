-- ~/.config/awesome/shell/bar/widgets.lua
local awful = require("awful")

local M = {}

local runtime = {
	api = {},
	cfg = {},
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return runtime.api or {}
end

local function widgets_api()
	return (api() and api().widgets) or {}
end

local function widget(name)
	return widgets_api()[name]
end

local function build_tabs(s, args)
	local WTabs = widget("tabs")
	local modkey = args.modkey
	local props = args.props or {}
	local tabs_theme = args.tabs_theme or {}
	local menu_api = args.menu_api

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
	local cfg = args.cfg or {}
	local show = (args.show == true)

	if not (show and WTags and type(WTags.build) == "function") then
		return nil
	end

	return WTags.build(s, {
		cfg = cfg,
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
	local cfg = args.cfg or {}
	local props = args.props or {}
	local bar_cfg = cfg.bar or {}
	local clock_cfg = bar_cfg.clock or {}

	if not (WClock and type(WClock.build) == "function") then
		return nil
	end

	return WClock.build(s, {
		show_seconds = (clock_cfg.show_seconds == true),
		app = cfg.apps and cfg.apps.calendar,
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
	local cfg = args.cfg or {}
	local props = args.props or {}
	local start_theme = args.start_theme or {}
	local show = (args.show == true)
	local start_action = args.start_action
	local menu_api = args.menu_api

	if not (show and WStart and type(WStart.build) == "function") then
		return nil
	end

	return WStart.build({
		screen = s,
		theme = start_theme,
		bar_height = props.height,
		cfg = cfg,
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

	runtime.api = args.api or {}
	runtime.cfg = args.cfg or {}
	runtime.ui = args.ui or {}

	return M
end

function M.build(s, args)
	args = args or {}

	return {
		tabs = build_tabs(s, {
			modkey = args.modkey,
			props = args.props,
			tabs_theme = args.tabs_theme,
			menu_api = args.menu_api,
		}),
		tags = build_tags(s, {
			cfg = args.cfg,
			show = (args.show_tags == true),
		}),
		tray = build_tray(s, {
			show = (args.showtray == true),
		}),
		clock = build_clock(s, {
			cfg = args.cfg,
			props = args.props,
		}),
		layoutbox = build_layoutbox(s, {}),
		start_btn = build_start(s, {
			cfg = args.cfg,
			props = args.props,
			start_theme = args.start_theme,
			show = (args.show_start == true),
			start_action = args.start_action,
			menu_api = args.menu_api,
		}),
		notify = build_notify(s, {
			show = (args.show_notify == true),
		}),
	}
end

return M
