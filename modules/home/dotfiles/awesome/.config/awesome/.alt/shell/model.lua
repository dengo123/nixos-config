-- features/shell/model.lua
-- Gibt { left, center, right } als fertige Widgets zurück

local wibox = require("wibox")
local awful = require("awful")

local M = {}

local function is_primary_screen(s)
	return s == screen.primary or s.index == 1
end

function M.build(s, opts)
	opts = opts or {}

	-- LEFT: fixes Layout (Instanz!)
	local left = wibox.layout.fixed.horizontal()
	left.spacing = 8
	if opts.launcher then
		left:add(opts.launcher)
	end
	if opts.keyboardlayout then
		left:add(opts.keyboardlayout)
	end

	-- CENTER: Tasklist mit echter Layout-Instanz
	local center = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = {},
		layout = wibox.layout.fixed.horizontal(), -- Instanz, KEINE Spec
		spacing = 6,
	})

	-- RIGHT: fixes Layout (Instanz!)
	local right = wibox.layout.fixed.horizontal()
	right.spacing = 8
	if opts.systray ~= false and is_primary_screen(s) then
		right:add(wibox.widget.systray())
	end
	right:add(wibox.widget.textclock("%a %d.%m. %H:%M"))

	return { left = left, center = center, right = right }
end

return M
