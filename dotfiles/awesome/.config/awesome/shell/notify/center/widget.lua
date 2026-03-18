-- ~/.config/awesome/shell/notify/center/widget.lua
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "notify.center.widget: " .. name .. " fehlt/ungueltig")
	return n
end

local function require_string(value, name)
	assert(type(value) == "string" and value ~= "", "notify.center.widget: " .. name .. " fehlt/ungueltig")
	return value
end

local function require_table(value, name)
	assert(type(value) == "table", "notify.center.widget: " .. name .. " fehlt/ungueltig")
	return value
end

local function notify_theme()
	return require_table(beautiful.notify, "beautiful.notify")
end

local function center_theme()
	local notify = notify_theme()
	return require_table(notify.center, "beautiful.notify.center")
end

local function resolve_theme()
	local notify = notify_theme()
	local center = center_theme()

	return {
		panel_bg = center.panel_bg or "#00000000",

		entry_bg = require_string(center.entry_bg or notify.bg, "beautiful.notify.center.entry_bg"),
		entry_fg = require_string(center.entry_fg or notify.fg, "beautiful.notify.center.entry_fg"),
		entry_border = require_string(center.entry_border or notify.border, "beautiful.notify.center.entry_border"),

		entry_radius = require_number(center.entry_radius or notify.radius, "beautiful.notify.center.entry_radius"),
		entry_border_w = require_number(
			center.entry_border_w or notify.border_w,
			"beautiful.notify.center.entry_border_w"
		),
		entry_padding = require_number(center.entry_padding or notify.margin, "beautiful.notify.center.entry_padding"),
		entry_spacing = require_number(center.entry_spacing or 10, "beautiful.notify.center.entry_spacing"),

		text_inset_top = require_number(center.text_inset_top or 2, "beautiful.notify.center.text_inset_top"),
		text_inset_bottom = require_number(center.text_inset_bottom or 2, "beautiful.notify.center.text_inset_bottom"),

		list_pad_top = require_number(center.list_pad_top or 0, "beautiful.notify.center.list_pad_top"),
		list_pad_right = require_number(center.list_pad_right or 0, "beautiful.notify.center.list_pad_right"),
		list_pad_bottom = require_number(center.list_pad_bottom or 0, "beautiful.notify.center.list_pad_bottom"),
		list_pad_left = require_number(center.list_pad_left or 0, "beautiful.notify.center.list_pad_left"),
	}
end

local function rounded_shape(radius)
	return function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, radius)
	end
end

local function entry_title(entry)
	local title = entry.title
	if title == nil or title == "" then
		title = entry.app_name or "Notification"
	end
	return title
end

local function entry_message(entry)
	return entry.message or entry.text or ""
end

local function make_textbox(theme, text, valign)
	local tb = wibox.widget({
		text = text or "",
		align = "left",
		valign = valign or "top",
		wrap = "word_char",
		widget = wibox.widget.textbox,
	})

	local inset = wibox.container.margin(tb)
	inset.top = theme.text_inset_top
	inset.bottom = theme.text_inset_bottom

	return inset
end

local function build_card(theme, title, message)
	local text_column = wibox.layout.fixed.vertical()
	text_column.spacing = 4
	text_column:add(make_textbox(theme, title, "center"))
	text_column:add(make_textbox(theme, message, "top"))

	local inner = wibox.container.margin(text_column)
	inner.left = theme.entry_padding
	inner.right = theme.entry_padding
	inner.top = theme.entry_padding + 2
	inner.bottom = theme.entry_padding + 2

	local background = wibox.container.background(inner)
	background.bg = theme.entry_bg
	background.fg = theme.entry_fg
	background.shape = rounded_shape(theme.entry_radius)
	background.shape_border_width = theme.entry_border_w
	background.shape_border_color = theme.entry_border

	return background
end

local function build_entry(theme, entry)
	return build_card(theme, entry_title(entry), entry_message(entry))
end

local function build_list(theme, entries)
	if not entries or #entries == 0 then
		return nil
	end

	local list = wibox.layout.fixed.vertical()
	list.spacing = theme.entry_spacing

	for _, entry in ipairs(entries) do
		list:add(build_entry(theme, entry))
	end

	return list
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(entries, _max_height)
	local theme = resolve_theme()
	local list = build_list(theme, entries)

	if not list then
		return nil
	end

	local padded = wibox.container.margin(list)
	padded.top = theme.list_pad_top
	padded.right = theme.list_pad_right
	padded.bottom = theme.list_pad_bottom
	padded.left = theme.list_pad_left

	local panel = wibox.container.background(padded)
	panel.bg = theme.panel_bg

	return panel
end

return M
