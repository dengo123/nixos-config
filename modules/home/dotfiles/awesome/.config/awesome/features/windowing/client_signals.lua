-- ~/.config/awesome/features/windowing/client_signals.lua
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local MinStack = require("features.windowing.minimize_stack")

local M = {}

-- Idempotenz-Guard: Signals nur 1x pro Session registrieren
if rawget(_G, "__windowing_signals_applied") then
	return M
end
_G.__windowing_signals_applied = true

-- Hilfen ---------------------------------------------------------------

local function safe_no_offscreen(c)
	if not (c and c.valid) then
		return
	end
	pcall(awful.placement.no_offscreen, c)
end

-- Maximized-Border konsistent halten: 0 wenn maximized/fullscreen, sonst Theme-Border
local function update_border_for_state(c)
	if not (c and c.valid) then
		return
	end
	local is_max = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal or c.maximized_fake
	local bw = is_max and 0 or (beautiful.border_width or 0)
	if c.border_width ~= bw then
		c.border_width = bw
	end
end

-- Titlebar nur 1x anhängen
local function ensure_titlebar_once(c, attach_fn, mouse, tb_opts)
	if not (c and c.valid) then
		return
	end
	if c._titlebar_attached then
		return
	end
	c._titlebar_attached = true
	attach_fn(c, mouse, tb_opts or {})
end

-- Haupt ----------------------------------------------------------------

function M.apply(o)
	local sloppy = o.sloppy_focus
	local taskbar = o.taskbar
	local mouse = o.mouse
	local tb_opts = o.titlebar_opts or { position = "top", size = 28 }

	-- Neue Clients on-screen platzieren (nur bei Autostart/ohne eigene Hints)
	client.connect_signal("manage", function(c)
		if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
			safe_no_offscreen(c)
		end
		update_border_for_state(c)
	end)

	-- Titlebars nur bei geeigneten Clients (normal/dialog)
	client.connect_signal("request::titlebars", function(c)
		local t = c.type
		if t == "normal" or t == "dialog" then
			ensure_titlebar_once(c, taskbar.attach, mouse, tb_opts)
		end
	end)

	-- Sloppy focus (ohne raise)
	if sloppy then
		client.connect_signal("mouse::enter", function(c)
			if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier and awful.client.focus.filter(c) then
				c:emit_signal("request::activate", "mouse_enter", { raise = false })
			end
		end)
	end

	-- Fokus/Unfokus: Borderfarben + urgent abbauen
	client.connect_signal("focus", function(c)
		if c and c.valid then
			c.urgent = false
			c.border_color = beautiful.border_focus
			update_border_for_state(c)
		end
	end)
	client.connect_signal("unfocus", function(c)
		if c and c.valid then
			c.border_color = beautiful.border_normal
			update_border_for_state(c)
		end
	end)

	-- Minimierungen tracken (für per-Screen Restore)
	client.connect_signal("property::minimized", function(c)
		if c.minimized then
			MinStack.push(c)
		else
			-- beim Restore: sicher gehen, dass er sichtbar & on-screen ist
			safe_no_offscreen(c)
			update_border_for_state(c)
			c:raise()
		end
	end)

	-- Konsistentes Border-Verhalten bei State-Wechseln
	client.connect_signal("property::maximized", update_border_for_state)
	client.connect_signal("property::maximized_horizontal", update_border_for_state)
	client.connect_signal("property::maximized_vertical", update_border_for_state)
	client.connect_signal("property::fullscreen", update_border_for_state)
	client.connect_signal("property::floating", update_border_for_state)

	-- Wenn der Screen wechselt (z. B. Monitor an/aus), nicht offscreen lassen
	client.connect_signal("property::screen", function(c)
		safe_no_offscreen(c)
		-- optional: pseudo_maximize-Flags neutralisieren, falls du die nutzt
		-- c.maximized_fake = false
		update_border_for_state(c)
	end)

	-- Bei Geometrie-/Workarea-Änderungen (z. B. Bar an/aus), Clients im Sichtbereich halten
	screen.connect_signal("property::workarea", function(s)
		for _, c in ipairs(s.clients) do
			safe_no_offscreen(c)
			update_border_for_state(c)
		end
	end)

	-- Aufräumen: verwaiste Einträge im Stack entfernen
	client.connect_signal("unmanage", function(c)
		MinStack.remove(c)
	end)
end

return M
