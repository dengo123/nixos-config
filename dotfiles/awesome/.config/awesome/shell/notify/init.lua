-- ~/.config/awesome/shell/notify/init.lua
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
}

local initialized = false
local center_open = false
local notify_callback_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function notify_cfg(cfg)
	return (cfg and cfg.notify) or {}
end

local function history_cfg(cfg)
	return notify_cfg(cfg).history or {}
end

local function center_cfg(cfg)
	return notify_cfg(cfg).center or {}
end

local function filter_cfg(cfg)
	return notify_cfg(cfg).filter or {}
end

local function require_notify_theme()
	return beautiful.notify or {}
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
	local Shape = mod("shape")

	if notify_cfg(cfg).speech == false then
		return Shape and Shape.rounded and Shape.rounded(notify_theme.radius) or nil
	end

	return Shape and Shape.speech and Shape.speech(notify_theme.radius) or nil
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

local function is_ignored_app(cfg, args)
	local apps = filter_cfg(cfg).apps or {}
	local app_name = tostring(args.app_name or args.appname or args.app or "")

	for _, name in ipairs(apps) do
		if app_name == tostring(name) then
			return true
		end
	end

	return false
end

local function should_store_notification(cfg, args)
	if args.skip_history == true then
		return false
	end

	local fcfg = filter_cfg(cfg)

	if fcfg.ignore_resident == true and args.resident == true then
		return false
	end

	if fcfg.ignore_silent == true and args.ignore == true then
		return false
	end

	if is_ignored_app(cfg, args) then
		return false
	end

	return true
end

local function emit_center_state()
	awesome.emit_signal("notify::center_state", center_open)
	awesome.emit_signal("ui::overlays_changed")
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

local function register_notify_callback(cfg)
	if notify_callback_ready then
		return
	end

	notify_callback_ready = true

	local History = mod("history")
	local prev_callback = naughty.config.notify_callback

	naughty.config.notify_callback = function(args)
		if type(prev_callback) == "function" then
			args = prev_callback(args) or args
		end

		args = sanitize_notify_args(args or {})

		if should_store_notification(cfg, args) and History and type(History.add) == "function" then
			History.add(args)
		end

		return args
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or args or {}
	local _ui = args.ui or {}

	if initialized then
		return M
	end

	M.api = {
		ui = _ui,
		theme = safe_require("shell.notify.theme"),
		center = safe_require("shell.notify.center"),
		history = safe_require("shell.notify.history"),
		rules = safe_require("shell.notify.rules"),
		shape = safe_require("shell.notify.shape"),
	}

	local Theme = mod("theme")
	local Center = mod("center")
	local History = mod("history")
	local Rules = mod("rules")

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			cfg = cfg,
			ui = _ui,
		})
	end

	local notify_theme = require_notify_theme()
	local shape_fn = resolve_shape(cfg, notify_theme)

	apply_defaults(cfg, notify_theme, shape_fn)
	apply_widget_template(notify_theme, shape_fn)
	apply_presets(cfg, notify_theme, shape_fn)

	if History and type(History.init) == "function" then
		History.init(history_cfg(cfg))
	end

	if Rules and type(Rules.apply) == "function" then
		Rules.apply()
	end

	if Center and type(Center.init) == "function" then
		Center.init({
			cfg = cfg,
			ui = _ui,
			history = History,
		})
	end

	register_center_signals()
	register_notify_callback(cfg)
	emit_center_state()

	initialized = true
	return M
end

function M.is_center_open()
	return center_open == true
end

function M.open_center()
	open_center()
end

function M.close_center()
	close_center()
end

function M.toggle_center()
	toggle_center()
end

return M
