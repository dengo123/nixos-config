-- ~/.config/awesome/shell/notify/init.lua
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")

local Center = require("shell.notify.center")
local History = require("shell.notify.history")
local Rules = require("shell.notify.rules")
local Shape = require("shell.notify.shape")

local M = {}

-- =========================================================================
-- State
-- =========================================================================

local initialized = false
local center_open = false
local notify_callback_ready = false

-- =========================================================================
-- Internal
-- =========================================================================

local function require_notify_theme()
	local notify = beautiful.notify

	assert(type(notify) == "table", "shell.notify: beautiful.notify fehlt/ungueltig")
	assert(notify.bg, "shell.notify: beautiful.notify.bg fehlt")
	assert(notify.fg, "shell.notify: beautiful.notify.fg fehlt")
	assert(notify.border, "shell.notify: beautiful.notify.border fehlt")
	assert(notify.radius, "shell.notify: beautiful.notify.radius fehlt")
	assert(notify.icon_size, "shell.notify: beautiful.notify.icon_size fehlt")
	assert(notify.margin, "shell.notify: beautiful.notify.margin fehlt")
	assert(notify.border_w, "shell.notify: beautiful.notify.border_w fehlt")

	return notify
end

local function notify_cfg(cfg)
	return cfg.notify or {}
end

local function resolve_position(cfg)
	local ncfg = notify_cfg(cfg)
	local bar_cfg = cfg.bar or {}

	if type(ncfg.position) == "string" and ncfg.position ~= "" then
		return ncfg.position
	end

	local bar_position = bar_cfg.position or "bottom"

	if bar_position == "top" then
		return "top_right"
	end

	return "bottom_right"
end

local function resolve_timeout(cfg)
	return tonumber(notify_cfg(cfg).timeout) or 3
end

local function resolve_shape(cfg, notify_theme)
	if notify_cfg(cfg).speech == false then
		return Shape.rounded(notify_theme.radius)
	end

	return Shape.speech(notify_theme.radius)
end

local function apply_defaults(cfg, notify_theme, shape_fn)
	naughty.config.defaults.position = resolve_position(cfg)
	naughty.config.defaults.timeout = resolve_timeout(cfg)
	naughty.config.defaults.margin = notify_theme.margin
	naughty.config.defaults.border_width = notify_theme.border_w
	naughty.config.defaults.border_color = notify_theme.border
	naughty.config.defaults.bg = notify_theme.bg
	naughty.config.defaults.fg = notify_theme.fg
	naughty.config.defaults.shape = shape_fn
	naughty.config.defaults.icon_size = notify_theme.icon_size
	naughty.config.defaults.width = tonumber(notify_theme.width)
end

local function apply_widget_template(notify_theme, shape_fn)
	naughty.config.defaults.widget_template = {
		{
			{
				{
					id = "title_role",
					align = "center",
					valign = "center",
					wrap = "word_char",
					widget = wibox.widget.textbox,
				},
				{
					id = "text_role",
					align = "center",
					valign = "center",
					wrap = "word_char",
					widget = wibox.widget.textbox,
				},
				spacing = 4,
				layout = wibox.layout.fixed.vertical,
			},
			margins = notify_theme.margin,
			widget = wibox.container.margin,
		},
		bg = notify_theme.bg,
		shape = shape_fn,
		widget = wibox.container.background,
	}
end

local function apply_presets(cfg, notify_theme, shape_fn)
	local timeout = resolve_timeout(cfg)

	for _, name in pairs({ "low", "normal", "critical" }) do
		local preset = naughty.config.presets[name]

		preset.bg = notify_theme.bg
		preset.fg = notify_theme.fg
		preset.border_width = notify_theme.border_w
		preset.border_color = notify_theme.border
		preset.shape = shape_fn
		preset.timeout = timeout
		preset.icon_size = notify_theme.icon_size
		preset.width = tonumber(notify_theme.width)
	end
end

local function sanitize_markup_text(value)
	if value == nil then
		return nil
	end

	return gears.string.xml_escape(tostring(value))
end

local function sanitize_notify_args(args)
	if type(args) ~= "table" then
		return args
	end

	args.title = sanitize_markup_text(args.title)
	args.text = sanitize_markup_text(args.text)
	args.message = sanitize_markup_text(args.message)
	args.app_name = sanitize_markup_text(args.app_name)

	return args
end

local function emit_center_state()
	awesome.emit_signal("notify::center_state", center_open)
end

local function open_center()
	if center_open then
		return
	end

	center_open = true
	emit_center_state()
end

local function close_center()
	if not center_open then
		return
	end

	center_open = false
	emit_center_state()
end

local function toggle_center()
	center_open = not center_open
	emit_center_state()
end

local function register_center_signals()
	awesome.connect_signal("notify::toggle_center", toggle_center)
	awesome.connect_signal("notify::open_center", open_center)
	awesome.connect_signal("notify::close_center", close_center)
end

local function register_notify_callback()
	if notify_callback_ready then
		return
	end

	notify_callback_ready = true

	local prev_callback = naughty.config.notify_callback

	naughty.config.notify_callback = function(args)
		if type(prev_callback) == "function" then
			args = prev_callback(args) or args
		end

		args = sanitize_notify_args(args or {})
		History.add(args)

		return args
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	if initialized then
		return
	end

	local notify_theme = require_notify_theme()
	local shape_fn = resolve_shape(cfg, notify_theme)

	apply_defaults(cfg, notify_theme, shape_fn)
	apply_widget_template(notify_theme, shape_fn)
	apply_presets(cfg, notify_theme, shape_fn)

	History.init(cfg.notify or {})
	Rules.apply()
	Center.init(cfg)

	register_center_signals()
	register_notify_callback()
	emit_center_state()

	initialized = true
end

return M
