-- ~/.config/awesome/shell/bar/wibar/controller.lua
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local M = {}

local runtime = {
	reveal_signals_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function empty_widget()
	return wibox.widget({
		widget = wibox.widget.separator,
		forced_width = 0,
		opacity = 0,
	})
end

local function resolve_visibility(policy, s, bar_cfg)
	local bar_normally_visible = true
	if policy and type(policy.bar_enabled_on_screen) == "function" then
		bar_normally_visible = (policy.bar_enabled_on_screen(s, bar_cfg) == true)
	end

	local tags_visible_on_screen = true
	if policy and type(policy.tags_enabled_on_screen) == "function" then
		tags_visible_on_screen = (policy.tags_enabled_on_screen(s, bar_cfg) == true)
	end

	local start_visible_on_screen = true
	if policy and type(policy.start_enabled_on_screen) == "function" then
		start_visible_on_screen = (policy.start_enabled_on_screen(s, bar_cfg) == true)
	end

	local notify_visible_on_screen = true
	if policy and type(policy.notify_enabled_on_screen) == "function" then
		notify_visible_on_screen = (policy.notify_enabled_on_screen(s, bar_cfg) == true)
	end

	return {
		bar = bar_normally_visible,
		tags = tags_visible_on_screen,
		start = start_visible_on_screen,
		notify = notify_visible_on_screen,
	}
end

local function resolve_start_action(policy, bar_cfg)
	local start_action = "menu"

	if policy and type(policy.start_action) == "function" then
		start_action = policy.start_action(bar_cfg)
	end

	return start_action
end

local function init_bar_ui(bar, conf, current_ui)
	local wibar_ui = bar.ui and bar.ui.wibar or nil
	local start_ui = bar.ui and bar.ui.start or nil
	local tabs_ui = bar.ui and bar.ui.tabs or nil

	if wibar_ui and type(wibar_ui.init) == "function" then
		wibar_ui.init({
			cfg = conf,
			ui = current_ui,
		})
	end

	if start_ui and type(start_ui.init) == "function" then
		start_ui.init({
			cfg = conf,
			ui = current_ui,
		})
	end

	if tabs_ui and type(tabs_ui.init) == "function" then
		tabs_ui.init({
			cfg = conf,
			ui = current_ui,
		})
	end

	local start_theme = (start_ui and type(start_ui.get) == "function" and start_ui.get()) or {}
	local tabs_theme = (tabs_ui and type(tabs_ui.get) == "function" and tabs_ui.get()) or {}

	if type(start_theme) ~= "table" then
		start_theme = {}
	end

	if type(tabs_theme) ~= "table" then
		tabs_theme = {}
	end

	return {
		wibar = wibar_ui,
		start = start_theme,
		tabs = tabs_theme,
	}
end

local function resolve_props(wibar_ui, bar_position)
	local props = (wibar_ui and type(wibar_ui.props) == "function" and wibar_ui.props()) or {}

	if type(props) ~= "table" then
		props = {}
	end

	props.height = tonumber(props.height) or tonumber(beautiful.wibar_height) or 28
	props.bg = props.bg or beautiful.wibar_bg or beautiful.bg_normal
	props.fg = props.fg or beautiful.wibar_fg or beautiful.fg_normal
	props.on_top = (props.on_top ~= nil) and props.on_top or beautiful.wibar_on_top
	props.opacity = tonumber(props.opacity) or tonumber(beautiful.wibar_opacity) or 1
	props.shape = props.shape or beautiful.wibar_shape
	props.margins = props.margins or beautiful.wibar_margins
	props.position = bar_position or "bottom"

	return props
end

local function build_sections(bar, parts, visibility)
	local sections_mod = bar.wibar and bar.wibar.sections or nil

	if sections_mod and type(sections_mod.build) == "function" then
		return sections_mod.build({
			show_start = visibility.start,
			show_tags = visibility.tags,
			show_notify = visibility.notify,
			start_btn = parts.start_btn or empty_widget(),
			tags = parts.tags or { indicator = empty_widget() },
			tabs = parts.tabs or { tasklist = empty_widget() },
			tray = parts.tray or empty_widget(),
			notify = parts.notify or empty_widget(),
			clock = parts.clock or empty_widget(),
			layoutbox = parts.layoutbox or empty_widget(),
		})
	end

	return {
		left = empty_widget(),
		center = nil,
		right = empty_widget(),
	}
end

local function create_wibar(s, props, sections, bar_normally_visible)
	local old = s.mywibar

	if old and old.valid then
		old.visible = false
		pcall(function()
			old:struts(nil)
		end)
		pcall(function()
			old.widget = nil
		end)
		s.mywibar = nil
	end

	s.mywibar = awful.wibar({
		position = props.position,
		screen = s,
		height = props.height,
		bg = props.bg,
		fg = props.fg,
		ontop = props.on_top,
		opacity = props.opacity,
		shape = props.shape,
		margins = props.margins,
		visible = bar_normally_visible,
	})

	s.mywibar._normal_visibility_enabled = bar_normally_visible
	s.mywibar._is_fixed_bar = (bar_normally_visible == true)
	s.mywibar._menu_anchor = (bar_normally_visible == true)

	s.mywibar:setup({
		layout = wibox.layout.align.horizontal,
		sections.left,
		sections.center,
		sections.right,
	})

	return s.mywibar
end

local function attach_reveal(bar, s, wibar, bar_cfg, props)
	local reveal = bar.behavior and bar.behavior.reveal or nil
	if not reveal then
		return
	end

	if not runtime.reveal_signals_ready and type(reveal.init_signals) == "function" then
		reveal.init_signals()
		runtime.reveal_signals_ready = true
	end

	if type(reveal.attach) == "function" then
		reveal.attach(s, wibar, {
			edge = props.position,
			trigger_px = tonumber(bar_cfg.reveal_trigger_px) or 2,
			hide_delay = tonumber(bar_cfg.reveal_hide_delay) or 0.20,
			layout_peek_duration = tonumber(bar_cfg.layout_peek_duration) or 0.70,
		})
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.setup(opts)
	opts = opts or {}

	local s = opts.screen
	if not s then
		return nil
	end

	local bar = opts.bar or {}
	local args = opts.args or {}

	local conf = args.cfg or {}
	local current_ui = args.ui or {}
	local menu_api = args.menu
	local notify_api = args.notify

	local bar_cfg = conf.bar or {}

	local showtray = (args.systray ~= false)

	local policy = bar.policy
	local widgets_mod = bar.wibar and bar.wibar.widgets or nil
	local group_tabs = bar.behavior and bar.behavior.group_tabs or nil

	local visibility = resolve_visibility(policy, s, bar_cfg)
	local start_action = resolve_start_action(policy, bar_cfg)

	local bar_ui = init_bar_ui(bar, conf, current_ui)
	local props = resolve_props(bar_ui.wibar, bar_cfg.position)

	local parts = {}

	if widgets_mod and type(widgets_mod.init) == "function" then
		widgets_mod.init({
			widgets = bar.widgets,
			cfg = conf,
			menu = menu_api,
			notify = notify_api,
			group_tabs = group_tabs,
		})
	end

	if widgets_mod and type(widgets_mod.build) == "function" then
		parts = widgets_mod.build(s, {
			cfg = conf,
			menu = menu_api,
			notify_api = notify_api,
			group_tabs = group_tabs,
			props = props,
			start_theme = bar_ui.start,
			tabs_theme = bar_ui.tabs,
			showtray = showtray,
			show_start = visibility.start,
			show_tags = visibility.tags,
			show_notify = visibility.notify,
			start_action = start_action,
		}) or {}
	end

	local sections = build_sections(bar, parts, visibility)
	local wibar = create_wibar(s, props, sections, visibility.bar)

	if parts.tray and s == (screen.primary or awful.screen.focused()) then
		awesome.emit_signal("ui::tray_ready", s)
	end

	attach_reveal(bar, s, wibar, bar_cfg, props)

	return wibar
end

return M
