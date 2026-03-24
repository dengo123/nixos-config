-- ~/.config/awesome/shell/bar/widgets/notify.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local History = require("shell.notify.history")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(s, _opts)
	local size = tonumber(beautiful.notify_button_size)
	local bg = beautiful.notify_button_bg
	local bg_hover = beautiful.notify_button_bg_hover
	local fg = beautiful.notify_button_fg
	local border_color = beautiful.notify_button_border_color
	local border_width = tonumber(beautiful.notify_button_border_width)
	local font = beautiful.notify_button_font
	local glyph_closed = beautiful.notify_button_glyph_closed
	local glyph_open = beautiful.notify_button_glyph_open
	local glyph_offset_y = tonumber(beautiful.notify_button_glyph_offset_y)
	local button_offset_y = tonumber(beautiful.notify_button_offset_y)

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

	local face = wibox.widget({
		glyph_box,
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

	local button_box = wibox.widget({
		button,
		top = math.max(0, button_offset_y),
		bottom = math.max(0, -button_offset_y),
		widget = wibox.container.margin,
	})

	-- ---------------------------------------------------------------------
	-- State
	-- ---------------------------------------------------------------------

	local function refresh_glyph()
		glyph.text = is_open and glyph_open or glyph_closed
	end

	-- ---------------------------------------------------------------------
	-- Signals
	-- ---------------------------------------------------------------------

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

	button:buttons(gears.table.join(
		awful.button({}, 1, function()
			awesome.emit_signal("notify::toggle_center")
		end),
		awful.button({}, 3, function()
			History.clear()
			awesome.emit_signal("notify::close_center")
		end)
	))

	refresh_glyph()

	return button_box
end

return M
