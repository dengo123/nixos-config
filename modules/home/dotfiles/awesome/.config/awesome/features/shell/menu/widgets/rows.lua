-- ~/.config/awesome/features/shell/menu/widgets/rows.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local theme = require("features.shell.menu.widgets.theme")
local helper = require("features.shell.menu.lib.helpers")
local Actions = require("features.shell.menu.lib.actions")

local M = {}

function M.row_widget(item, t, opts)
	t = theme.with_defaults(t)
	opts = opts or {}

	local eff_h = t.row_h
	local pad_t = t.row_pad_t or 0
	local pad_b = t.row_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = theme.resolve_icon_size(t, avail_h, "rows")
	local font = theme.resolve_font(t, avail_h, "rows")

	local hline = wibox.widget({
		{
			image = item.icon,
			resize = true,
			forced_height = icon_px,
			forced_width = icon_px,
			widget = wibox.widget.imagebox,
		},
		{ text = item.text or "", font = font, valign = "center", widget = wibox.widget.textbox },
		spacing = t.row_spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	local placed = wibox.widget({
		hline,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local content = wibox.widget({
		placed,
		left = t.row_pad_l,
		right = t.row_pad_r,
		top = pad_t,
		bottom = pad_b,
		widget = wibox.container.margin,
	})

	local bg_box = wibox.widget({
		content,
		bg = t.row_bg,
		fg = t.row_fg,
		widget = wibox.container.background,
	})

	helper.apply_hover(bg_box, t, t.row_bg, t.row_bg_hover)

	-- Klick strikt zentral über Actions (Dialoge/Aktionen entscheidet lib/actions.lua)
	bg_box:buttons(gears.table.join(awful.button({}, 1, Actions.click(item))))

	-- Optionale Hover-Callbacks aus Item beibehalten
	bg_box:connect_signal("mouse::enter", function()
		if item.on_hover_in then
			item.on_hover_in(item, bg_box)
		end
	end)
	bg_box:connect_signal("mouse::leave", function()
		if item.on_hover_out then
			item.on_hover_out(item, bg_box)
		end
	end)

	return helper.fixed_height(bg_box, eff_h)
end

function M.list_widget(items, t, opts)
	t = theme.with_defaults(t)
	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing }
	for _, it in ipairs(items or {}) do
		table.insert(box, M.row_widget(it, t, opts))
	end
	return wibox.widget(box)
end

return M
