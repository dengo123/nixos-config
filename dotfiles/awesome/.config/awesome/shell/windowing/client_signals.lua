-- ~/.config/awesome/shell/windowing/client_signals.lua
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local MinStack = require("shell.windowing.minimize_stack")

local M = {}

if rawget(_G, "__windowing_signals_applied") then
	return M
end
_G.__windowing_signals_applied = true

-- ===== Helpers =====
local function safe_no_offscreen(c)
	if not (c and c.valid) then
		return
	end
	pcall(awful.placement.no_offscreen, c)
end

local function get_color(key, fallback)
	local v = beautiful[key]
	return (type(v) == "string" and #v > 0) and v or fallback
end

local BORDER_FOCUS = get_color("border_focus", "#4C6EF5") -- Default: blau
local BORDER_NORMAL = get_color("border_normal", "#444444") -- Default: grau

local function set_border_color(c, color)
	if not (c and c.valid) then
		return
	end
	if type(color) == "string" then
		c.border_color = color
	end
end

local function update_border_for_state(c)
	if not (c and c.valid) then
		return
	end
	local is_max = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal or c.maximized_fake
	local bw = is_max and 0 or (tonumber(beautiful.border_width) or 0)
	if c.border_width ~= bw then
		c.border_width = bw
	end
end

local function ensure_titlebar_once(c, attach_fn, mouse, tb_opts)
	if not (c and c.valid) then
		return
	end
	if c._titlebar_attached then
		return
	end
	c._titlebar_attached = true
	if type(attach_fn) == "function" then
		attach_fn(c, mouse, tb_opts or {})
	end
end

-- ===== Main =====
function M.apply(o)
	local sloppy = o.sloppy_focus
	local taskbar = o.taskbar
	local mouse = o.mouse
	local tb_opts = o.titlebar_opts or { position = "top", size = 28 }

	client.connect_signal("manage", function(c)
		if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
			safe_no_offscreen(c)
		end
		update_border_for_state(c)
	end)

	client.connect_signal("request::titlebars", function(c)
		local t = c.type
		if t == "normal" or t == "dialog" then
			ensure_titlebar_once(c, taskbar and taskbar.attach, mouse, tb_opts)
		end
	end)

	if sloppy then
		client.connect_signal("mouse::enter", function(c)
			if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier and awful.client.focus.filter(c) then
				c:emit_signal("request::activate", "mouse_enter", { raise = false })
			end
		end)
	end

	client.connect_signal("focus", function(c)
		if c and c.valid then
			c.urgent = false
			set_border_color(c, get_color("border_focus", BORDER_FOCUS))
			update_border_for_state(c)
		end
	end)

	client.connect_signal("unfocus", function(c)
		if c and c.valid then
			set_border_color(c, get_color("border_normal", BORDER_NORMAL))
			update_border_for_state(c)
		end
	end)

	client.connect_signal("property::minimized", function(c)
		if c.minimized then
			MinStack.push(c)
		else
			safe_no_offscreen(c)
			update_border_for_state(c)
			c:raise()
		end
	end)

	client.connect_signal("property::maximized", update_border_for_state)
	client.connect_signal("property::maximized_horizontal", update_border_for_state)
	client.connect_signal("property::maximized_vertical", update_border_for_state)
	client.connect_signal("property::fullscreen", update_border_for_state)
	client.connect_signal("property::floating", update_border_for_state)

	client.connect_signal("property::screen", function(c)
		safe_no_offscreen(c)
		update_border_for_state(c)
	end)

	screen.connect_signal("property::workarea", function(s)
		for _, c in ipairs(s.clients) do
			safe_no_offscreen(c)
			update_border_for_state(c)
		end
	end)

	client.connect_signal("unmanage", function(c)
		MinStack.remove(c)
	end)
end

return M
