-- ~/.config/awesome/shell/bar/widgets/tags.lua
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

function M.build(s, opts)
	opts = opts or {}
	local S = beautiful.tags_indicator or {}

	local pad_h = opts.pad_h or S.pad_h or 6
	local pad_v = opts.pad_v or S.pad_v or 0
	local font = opts.font or S.font or beautiful.font
	local fmt = opts.format or S.fmt or "%d"

	local collapsed_pad_h = S.collapsed_pad_h or 6

	local text = wibox.widget({
		widget = wibox.widget.textbox,
		font = font,
		align = "center",
		valign = "center",
	})

	local indicator = wibox.widget({
		text,
		widget = wibox.container.margin,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
	})

	local function set_normal_margins()
		indicator.left, indicator.right = pad_h, pad_h
		indicator.top, indicator.bottom = pad_v, pad_v
	end

	local function set_collapsed_spacing()
		-- keine Zahl anzeigen, aber fixen Abstand erzwingen
		text.text = ""
		indicator.left, indicator.right = collapsed_pad_h, collapsed_pad_h
		indicator.top, indicator.bottom = pad_v, pad_v
		indicator.forced_width = 2 * collapsed_pad_h -- << Schlüssel!
		indicator.visible = true
	end

	local function clear_forced_width()
		indicator.forced_width = nil
	end

	local function refresh()
		local tags = s.tags or {}
		if (tags and #tags or 0) <= 1 then
			set_collapsed_spacing()
			return
		end

		clear_forced_width()
		set_normal_margins()
		indicator.visible = true

		local t = s.selected_tag or awful.screen.focused().selected_tag
		local idx = (t and t.index) or 1
		text.text = string.format(fmt, idx)
	end

	refresh()

	awful.tag.attached_connect_signal(s, "property::selected", refresh)
	awful.tag.attached_connect_signal(s, "property::name", refresh)
	awful.tag.attached_connect_signal(s, "tagged", refresh)
	awful.tag.attached_connect_signal(s, "untagged", refresh)
	awful.tag.attached_connect_signal(s, "property::activated", refresh)
	s:connect_signal("tag::history::update", refresh)

	return { indicator = indicator }
end

return M
