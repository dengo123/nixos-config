-- ~/.config/awesome/shell/bar/widgets/notify.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "notify widget: " .. name .. " fehlt/ungültig")
	return n
end

local function require_string(value, name)
	assert(type(value) == "string" and value ~= "", "notify widget: " .. name .. " fehlt/ungültig")
	return value
end

function M.build(_s, _opts)
	local size = require_number(beautiful.notify_button_size, "beautiful.notify_button_size")
	local bg = require_string(beautiful.notify_button_bg, "beautiful.notify_button_bg")
	local bg_hover = require_string(beautiful.notify_button_bg_hover, "beautiful.notify_button_bg_hover")
	local fg = require_string(beautiful.notify_button_fg, "beautiful.notify_button_fg")
	local border_color = require_string(beautiful.notify_button_border_color, "beautiful.notify_button_border_color")
	local border_width = require_number(beautiful.notify_button_border_width, "beautiful.notify_button_border_width")
	local font = require_string(beautiful.notify_button_font, "beautiful.notify_button_font")
	local glyph_closed = require_string(beautiful.notify_button_glyph_closed, "beautiful.notify_button_glyph_closed")
	local glyph_open = require_string(beautiful.notify_button_glyph_open, "beautiful.notify_button_glyph_open")
	local glyph_offset_y = tonumber(beautiful.notify_button_glyph_offset_y) or 0
	local badge_font = beautiful.notify_button_badge_font or font

	local is_open = false

	local glyph = wibox.widget({
		text = glyph_closed,
		align = "center",
		valign = "center",
		font = font,
		widget = wibox.widget.textbox,
	})

	local glyph_box = wibox.widget({
		glyph,
		top = math.max(0, glyph_offset_y),
		bottom = math.max(0, -glyph_offset_y),
		widget = wibox.container.margin,
	})

	local unread = wibox.widget({
		text = "",
		align = "center",
		valign = "center",
		visible = false,
		font = badge_font,
		widget = wibox.widget.textbox,
	})

	local content = wibox.widget({
		glyph_box,
		unread,
		layout = wibox.layout.stack,
	})

	local face = wibox.widget({
		content,
		forced_width = size,
		forced_height = size,
		bg = bg,
		fg = fg,
		shape = gears.shape.circle,
		shape_border_width = border_width,
		shape_border_color = border_color,
		widget = wibox.container.background,
	})

	local button = wibox.widget({
		face,
		strategy = "exact",
		width = size,
		height = size,
		widget = wibox.container.constraint,
	})

	local function refresh_glyph()
		glyph.text = is_open and glyph_open or glyph_closed
	end

	local function refresh()
		refresh_glyph()
	end

	awesome.connect_signal("notify::center_state", function(open)
		is_open = (open == true)
		refresh_glyph()
	end)

	button:connect_signal("mouse::enter", function()
		face.bg = bg_hover
	end)

	button:connect_signal("mouse::leave", function()
		face.bg = bg
	end)

	button:buttons(gears.table.join(awful.button({}, 1, function()
		awesome.emit_signal("notify::toggle_center")
	end)))

	refresh()

	return button
end

return M
