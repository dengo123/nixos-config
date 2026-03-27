-- ~/.config/awesome/shell/notify/ui/naughty.lua
local beautiful = require("beautiful")
local naughty = require("naughty")
local wibox = require("wibox")

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function notify_cfg(cfg)
	return (cfg and cfg.notify) or {}
end

local function require_notify_theme()
	return beautiful.notify or {}
end

local function resolve_position(cfg)
	local ncfg = notify_cfg(cfg)
	local bar_cfg = (cfg and cfg.bar) or {}

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

local function resolve_shape(cfg, Shape, notify_theme)
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

	naughty.config.padding = tonumber(notify_theme.padding)
	naughty.config.spacing = tonumber(notify_theme.spacing)
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

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	args = args or {}

	local cfg = args.cfg or ctx().cfg or {}
	local Shape = args.shape
	local notify_theme = args.notify_theme or require_notify_theme()
	local shape_fn = resolve_shape(cfg, Shape, notify_theme)

	apply_defaults(cfg, notify_theme, shape_fn)
	apply_widget_template(notify_theme, shape_fn)
	apply_presets(cfg, notify_theme, shape_fn)

	return M
end

return M
