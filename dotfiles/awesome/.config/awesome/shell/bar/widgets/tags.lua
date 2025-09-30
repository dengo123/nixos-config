local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

function M.build(s, opts)
	opts = opts or {}
	local pad_h = opts.pad_h or 6
	local pad_v = opts.pad_v or 0
	local font = opts.font or beautiful.font
	local fmt = opts.format or "%d" -- wie die Zahl formatiert wird

	local text = wibox.widget({
		widget = wibox.widget.textbox,
		font = font,
		align = "center",
		valign = "center",
	})

	-- kein Button-Style: nur Margin drumherum (optional)
	local indicator = wibox.widget({
		text,
		widget = wibox.container.margin,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
	})

	local function refresh()
		local tags = s.tags or {}
		if #tags <= 1 then
			indicator.visible = false
			return
		end
		indicator.visible = true
		local t = s.selected_tag or awful.screen.focused().selected_tag
		local idx = (t and t.index) or 1
		text.text = string.format(fmt, idx)
	end

	-- initial
	refresh()

	-- auf Tag-Änderungen reagieren (Auswahl, Namen, Tagging…)
	awful.tag.attached_connect_signal(s, "property::selected", refresh)
	awful.tag.attached_connect_signal(s, "property::name", refresh)
	awful.tag.attached_connect_signal(s, "tagged", refresh)
	awful.tag.attached_connect_signal(s, "untagged", refresh)
	awful.tag.attached_connect_signal(s, "property::activated", refresh)
	s:connect_signal("tag::history::update", refresh)

	return { indicator = indicator }
end

return M
