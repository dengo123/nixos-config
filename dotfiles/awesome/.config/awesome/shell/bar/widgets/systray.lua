-- ~/.config/awesome/widgets/bar/systray.lua
local beautiful = require("beautiful")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "systray: " .. name .. " fehlt/ungültig")
	return n
end

local function require_string(value, name)
	assert(type(value) == "string" and value ~= "", "systray: " .. name .. " fehlt/ungültig")
	return value
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local bar_height = require_number(beautiful.wibar_height, "beautiful.wibar_height")
	local theme_pad_h = require_number(beautiful.systray_pad_h, "beautiful.systray_pad_h")
	local theme_pad_v = require_number(beautiful.systray_pad_v, "beautiful.systray_pad_v")
	local theme_base_size = require_number(beautiful.systray_base_size, "beautiful.systray_base_size")
	local theme_bg = require_string(beautiful.systray_bg, "beautiful.systray_bg")

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local pad_h = tonumber(opts.pad_h) or theme_pad_h
	local pad_v = tonumber(opts.pad_v) or theme_pad_v
	local base_size = tonumber(opts.base_size) or theme_base_size
	local bg = opts.bg or theme_bg

	-- ---------------------------------------------------------------------
	-- Tray
	-- ---------------------------------------------------------------------

	local tray = wibox.widget.systray()
	tray:set_horizontal(true)
	tray:set_base_size(base_size)

	local placed = wibox.widget({
		tray,
		widget = wibox.container.place,
		halign = "left",
		valign = "center",
	})

	local with_margin = wibox.widget({
		placed,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
		widget = wibox.container.margin,
	})

	local boxed = wibox.widget({
		with_margin,
		bg = bg,
		widget = wibox.container.background,
	})

	return wibox.container.constraint(boxed, "exact", nil, bar_height)
end

return M
