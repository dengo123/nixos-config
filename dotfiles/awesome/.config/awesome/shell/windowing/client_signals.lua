-- ~/.config/awesome/shell/windowing/client_signals.lua
local awful = require("awful")
local gears = require("gears")
local MinStack = require("shell.windowing.minimize_stack")

local M = {}

-- Mehrfaches Laden verhindern
if rawget(_G, "__windowing_signals_applied") then
	return M
end
_G.__windowing_signals_applied = true

-- ======================
-- Helpers
-- ======================

local function safe_no_offscreen(c)
	if not (c and c.valid) then
		return
	end
	pcall(awful.placement.no_offscreen, c)
end

local function ensure_titlebar_once(c, attach_fn, mouse, tb_opts)
	if not (c and c.valid) or c._titlebar_attached then
		return
	end
	c._titlebar_attached = true
	if type(attach_fn) == "function" then
		attach_fn(c, mouse, tb_opts or {})
	end
end

-- Fallback: Maus-Bindings für Titlebar (LMB = Move, RMB = Resize)
local function default_titlebar_buttons(c)
	return gears.table.join(
		awful.button({}, 1, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)
end

-- ======================
-- Apply
-- ======================

function M.apply(o)
	local sloppy = o.sloppy_focus
	local taskbar = o.taskbar -- { attach = ... } vom Theme
	local mouse = o.mouse or {}
	local tb_opts = o.titlebar_opts or { position = "top", size = 28 }
	local style = o.style -- Theme-Hook: apply_client_style(c)

	-- >>> Sichere Titlebar-Buttons bereitstellen (für Drag/Resize)
	if type(mouse.titlebar_buttons) ~= "function" then
		mouse.titlebar_buttons = function(c)
			return default_titlebar_buttons(c)
		end
	end

	local function restyle(c)
		if style and style.apply_client_style then
			style.apply_client_style(c)
		end
	end

	client.connect_signal("manage", function(c)
		if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
			safe_no_offscreen(c)
		end
		restyle(c)
	end)

	client.connect_signal("request::titlebars", function(c)
		local t = c.type
		if t == "normal" or t == "dialog" then
			ensure_titlebar_once(c, taskbar and taskbar.attach, mouse, tb_opts)
			restyle(c)
		end
	end)

	if sloppy then
		client.connect_signal("mouse::enter", function(c)
			if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier and awful.client.focus.filter(c) then
				c:emit_signal("request::activate", "mouse_enter", { raise = false })
			end
		end)
	end

	client.connect_signal("focus", restyle)
	client.connect_signal("unfocus", restyle)

	client.connect_signal("property::minimized", function(c)
		if c.minimized then
			MinStack.push(c)
		else
			safe_no_offscreen(c)
			restyle(c)
			c:raise()
		end
	end)

	client.connect_signal("property::maximized", restyle)
	client.connect_signal("property::maximized_horizontal", restyle)
	client.connect_signal("property::maximized_vertical", restyle)
	client.connect_signal("property::fullscreen", restyle)
	client.connect_signal("property::floating", restyle)

	client.connect_signal("property::screen", function(c)
		safe_no_offscreen(c)
		restyle(c)
	end)

	screen.connect_signal("property::workarea", function(s)
		for _, c in ipairs(s.clients) do
			safe_no_offscreen(c)
			restyle(c)
		end
	end)

	client.connect_signal("unmanage", function(c)
		MinStack.remove(c)
	end)
end

return M
