local wibox = require("wibox")
local gears = require("gears")
local W = require("features.shell.menu.widgets")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

function M.build(th, dims, w)
	-- Header
	local header = wibox.widget({
		{
			W.mk_header_content(w.title or "", th),
			left = pick(th.header_pad_h, th.pad_h, 12),
			right = pick(th.header_pad_h, th.pad_h, 12),
			widget = wibox.container.margin,
		},
		bg = pick(th.header_bg, "#235CDB"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	local body = wibox.widget({
		{
			{ w.body, halign = "center", valign = "center", widget = wibox.container.place },
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#00000000"),
		fg = pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	local footer_right = wibox.widget({
		{ w.cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
		right = pick(th.footer_pad_h, th.pad_h, 12),
		widget = wibox.container.margin,
	})

	local footer = wibox.widget({
		{
			footer_right,
			top = pick(th.footer_pad_v, 8),
			bottom = pick(th.footer_pad_v, 8),
			widget = wibox.container.margin,
		},
		bg = pick(th.footer_bg, "#235CDB"),
		fg = pick(th.footer_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Fixhöhen
	return wibox.widget({
		{
			{ header, strategy = "exact", height = dims.header_h, widget = wibox.container.constraint },
			{ body, strategy = "exact", height = dims.body_h, widget = wibox.container.constraint },
			{ footer, strategy = "exact", height = dims.footer_h, widget = wibox.container.constraint },
			layout = wibox.layout.fixed.vertical,
		},
		shape = gears.shape.rounded_rect,
		bg = pick(th.dialog_bg, "#00000000"),
		widget = wibox.container.background,
	})
end

return M
