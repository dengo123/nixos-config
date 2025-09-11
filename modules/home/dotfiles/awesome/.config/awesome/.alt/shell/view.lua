-- features/shell/view.lua
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

-- Nur echte Spezifikationen (mit layout/widget-Key) bauen, fertige Widgets unverändert lassen
local function to_widget(spec)
	if not spec then
		return nil
	end
	if type(spec) == "table" and (spec.layout ~= nil or spec.widget ~= nil) then
		return wibox.widget(spec)
	end
	return spec
end

function M.apply(s, sections, opts)
	opts = opts or {}
	local pos = opts.position or beautiful.wibar_position or "bottom"
	local height = opts.height or beautiful.wibar_height or 28
	local bg = beautiful.wibar_bg or beautiful.bg_normal or "#000000aa"
	local fg = beautiful.wibar_fg or beautiful.fg_normal or "#ffffff"

	if s.mywibox and s.mywibox.remove then
		s.mywibox:remove()
	end

	s.mywibox = awful.wibar({
		screen = s,
		position = pos,
		height = height,
		bg = bg,
		fg = fg,
		visible = true,
	})

	local l = to_widget(sections and sections.left) or wibox.widget.textbox(" ")
	local c = to_widget(sections and sections.center) or wibox.widget.textbox(" ")
	local r = to_widget(sections and sections.right) or wibox.widget.textbox(" ")

	if pos == "left" or pos == "right" then
		local layout = wibox.layout.align.vertical()
		layout:set_top(l)
		layout:set_middle(c)
		layout:set_bottom(r)
		s.mywibox.widget = layout
	else
		local layout = wibox.layout.align.horizontal()
		layout:set_left(l)
		layout:set_middle(c)
		layout:set_right(r)
		s.mywibox.widget = layout
	end
end

return M
