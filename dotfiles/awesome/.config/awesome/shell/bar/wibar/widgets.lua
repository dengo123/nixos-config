-- ~/.config/awesome/shell/bar/wibar/widgets.lua
local awful = require("awful")

local M = {}

local runtime = {
	widgets = {},
	cfg = {},
	menu = nil,
	notify = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function widgets()
	return runtime.widgets or {}
end

local function widget(name)
	return widgets()[name]
end

local function cfg()
	return runtime.cfg or {}
end

local function menu()
	return runtime.menu
end

local function notify_api()
	return runtime.notify
end

local function needs_shell(cmd)
	if type(cmd) ~= "string" then
		return false
	end

	return cmd:find("[%s][\"']")
		or cmd:find("||", 1, true)
		or cmd:find("&&", 1, true)
		or cmd:find("~", 1, true)
		or cmd:find("%$")
		or cmd:find("[<>|;&]")
end

local function spawn_cmd(cmd)
	if type(cmd) == "table" then
		awful.spawn(cmd)
		return true
	end

	if type(cmd) == "string" and cmd ~= "" then
		if needs_shell(cmd) then
			awful.spawn.with_shell(cmd)
		else
			awful.spawn(cmd)
		end
		return true
	end

	return false
end

-- =========================================================================
-- Start Actions
-- =========================================================================

local function toggle_start_menu(s, btn, menu_api_ref)
	if not menu_api_ref then
		return
	end

	if type(menu_api_ref.is_open) == "function" and menu_api_ref.is_open() then
		if type(menu_api_ref.hide) == "function" then
			menu_api_ref.hide()
		end
		return
	end

	if type(menu_api_ref.show_for_start_widget) == "function" then
		menu_api_ref.show_for_start_widget(s, btn)
	end
end

local function build_start_click_handlers(s, conf, start_action, menu_api_ref)
	local apps_cfg = conf.apps or {}
	local action = tostring(start_action or "menu"):lower()

	local function toggle(ref)
		toggle_start_menu((ref and ref.screen) or s, (ref and ref.widget) or nil, menu_api_ref)
	end

	local function on_left_click(ref)
		if action == "menu" then
			toggle(ref)
			return
		end

		if action == "rofi" then
			awful.spawn.with_shell("rofi -show drun")
			return
		end

		if action == "editor" then
			if spawn_cmd(apps_cfg.editor) then
				return
			end
			toggle(ref)
			return
		end

		if action == "terminal" then
			if spawn_cmd(apps_cfg.terminal) then
				return
			end
			toggle(ref)
			return
		end

		toggle(ref)
	end

	local function on_right_click(ref)
		toggle(ref)
	end

	return {
		on_left_click = on_left_click,
		on_right_click = on_right_click,
	}
end

-- =========================================================================
-- Tabs Hooks
-- =========================================================================

local function build_tabs_hooks(s, menu_api_ref)
	local function on_left_click(ref)
		local clients = (ref and ref.clients) or {}
		local focused = client.focus

		if focused then
			for _, c in ipairs(clients) do
				if c == focused and c.valid then
					c.minimized = true
					return
				end
			end
		end

		local lead = ref and ref.lead or nil
		if lead and lead.valid then
			lead:emit_signal("request::activate", "group_tab", { raise = true })
		end
	end

	local function on_right_click(ref)
		if not menu_api_ref then
			return
		end

		if type(menu_api_ref.is_open) == "function" and menu_api_ref.is_open() then
			if type(menu_api_ref.hide) == "function" then
				menu_api_ref.hide()
			end
			return
		end

		if type(menu_api_ref.show_for_tabs_widget_with_clients_at) ~= "function" then
			return
		end

		local clients = (ref and ref.clients) or {}
		if #clients == 0 then
			return
		end

		menu_api_ref.show_for_tabs_widget_with_clients_at(s, ref.widget, clients, ref.anchor)
	end

	local function on_scroll_up(_ref)
		awful.client.focus.byidx(1)
	end

	local function on_scroll_down(_ref)
		awful.client.focus.byidx(-1)
	end

	return {
		on_left_click = on_left_click,
		on_right_click = on_right_click,
		on_scroll_up = on_scroll_up,
		on_scroll_down = on_scroll_down,
	}
end

-- =========================================================================
-- Widget Builders
-- =========================================================================

-- -------------------------------------------------------------------------
-- Tabs
-- -------------------------------------------------------------------------

local function build_tabs(s, opts)
	local tabs_widget = widget("tabs")
	local props = opts.props or {}
	local tabs_theme = opts.tabs_theme or {}
	local menu_api_ref = opts.menu
	local group_tabs = opts.group_tabs

	if not (tabs_widget and type(tabs_widget.build) == "function") then
		return nil
	end

	local hooks = build_tabs_hooks(s, menu_api_ref)

	local function entries_fn()
		if group_tabs and type(group_tabs.collect) == "function" then
			return group_tabs.collect(s, {
				cfg = opts.cfg or cfg(),
			})
		end

		return {}
	end

	return tabs_widget.build(s, {
		theme = tabs_theme,
		bar_height = props.height,
		entries_fn = entries_fn,
		on_left_click = hooks.on_left_click,
		on_right_click = hooks.on_right_click,
		on_scroll_up = hooks.on_scroll_up,
		on_scroll_down = hooks.on_scroll_down,
	})
end

-- -------------------------------------------------------------------------
-- Tags
-- -------------------------------------------------------------------------

local function build_tags(s, opts)
	local tags_widget = widget("tags")
	local conf = opts.cfg or cfg()
	local show = (opts.show == true)

	if not (show and tags_widget and type(tags_widget.build) == "function") then
		return nil
	end

	return tags_widget.build(s, {
		cfg = conf,
	})
end

-- -------------------------------------------------------------------------
-- Tray
-- -------------------------------------------------------------------------

local function build_tray(_s, opts)
	local systray_widget = widget("systray")
	local show = (opts.show == true)

	if not (show and systray_widget and type(systray_widget.build) == "function") then
		return nil
	end

	return systray_widget.build({})
end

-- -------------------------------------------------------------------------
-- Clock
-- -------------------------------------------------------------------------

local function build_clock(s, opts)
	local clock_widget = widget("clock")
	local conf = opts.cfg or cfg()
	local props = opts.props or {}
	local bar_cfg = conf.bar or {}
	local clock_cfg = bar_cfg.clock or {}

	if not (clock_widget and type(clock_widget.build) == "function") then
		return nil
	end

	return clock_widget.build(s, {
		show_seconds = (clock_cfg.show_seconds == true),
		app = conf.apps and conf.apps.calendar,
		calendar_enable = (clock_cfg.calendar_enable ~= false),
		calendar_use_menu_theme = (clock_cfg.calendar_use_menu_theme == true),
		bar_position = props.position,
	})
end

-- -------------------------------------------------------------------------
-- Layoutbox
-- -------------------------------------------------------------------------

local function build_layoutbox(s)
	local layoutbox_widget = widget("layoutbox")

	if not (layoutbox_widget and type(layoutbox_widget.build) == "function") then
		return nil
	end

	return layoutbox_widget.build(s)
end

-- -------------------------------------------------------------------------
-- Start
-- -------------------------------------------------------------------------

local function build_start(s, opts)
	local start_widget = widget("start")
	local conf = opts.cfg or cfg()
	local props = opts.props or {}
	local start_theme = opts.start_theme or {}
	local show = (opts.show == true)
	local start_action = opts.start_action
	local menu_api_ref = opts.menu

	if not (show and start_widget and type(start_widget.build) == "function") then
		return nil
	end

	local handlers = build_start_click_handlers(s, conf, start_action, menu_api_ref)

	return start_widget.build({
		screen = s,
		theme = start_theme,
		bar_height = props.height,
		on_left_click = handlers.on_left_click,
		on_right_click = handlers.on_right_click,
	})
end

-- -------------------------------------------------------------------------
-- Notify
-- -------------------------------------------------------------------------

local function build_notify(s, opts)
	local notify_widget = widget("notify")
	local show = (opts.show == true)
	local notify = opts.notify_api or notify_api()

	if not (show and notify_widget and type(notify_widget.build) == "function") then
		return nil
	end

	if type(notify) ~= "table" then
		return nil
	end

	return notify_widget.build(s, {
		notify_api = notify,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	runtime.widgets = opts.widgets or runtime.widgets
	runtime.cfg = opts.cfg or runtime.cfg
	runtime.menu = opts.menu or runtime.menu
	runtime.notify = opts.notify or runtime.notify

	return M
end

function M.build(s, opts)
	opts = opts or {}

	local conf = opts.cfg or cfg()
	local menu_api_ref = opts.menu or menu()
	local notify = opts.notify_api or notify_api()

	return {
		tabs = build_tabs(s, {
			cfg = conf,
			props = opts.props,
			tabs_theme = opts.tabs_theme,
			menu = menu_api_ref,
			group_tabs = opts.group_tabs,
		}),
		tags = build_tags(s, {
			cfg = conf,
			show = (opts.show_tags == true),
		}),
		tray = build_tray(s, {
			show = (opts.showtray == true),
		}),
		clock = build_clock(s, {
			cfg = conf,
			props = opts.props,
		}),
		layoutbox = build_layoutbox(s),
		start_btn = build_start(s, {
			cfg = conf,
			props = opts.props,
			start_theme = opts.start_theme,
			show = (opts.show_start == true),
			start_action = opts.start_action,
			menu = menu_api_ref,
		}),
		notify = build_notify(s, {
			show = (opts.show_notify == true),
			notify_api = notify,
		}),
	}
end

return M
