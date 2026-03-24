-- ~/.config/awesome/shell/bar/widgets/systray.lua
local beautiful = require("beautiful")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local bar_height = tonumber(beautiful.wibar_height)
	local theme_pad_h = tonumber(beautiful.systray_pad_h)
	local theme_pad_v = tonumber(beautiful.systray_pad_v)
	local theme_base_size = tonumber(beautiful.systray_base_size)

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local pad_h = tonumber(opts.pad_h) or theme_pad_h
	local pad_v = tonumber(opts.pad_v) or theme_pad_v
	local base_size = tonumber(opts.base_size) or theme_base_size

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

	return wibox.container.constraint(with_margin, "exact", nil, bar_height)
end

return M
