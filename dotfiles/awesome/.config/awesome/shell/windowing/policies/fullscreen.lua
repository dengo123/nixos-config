-- ~/.config/awesome/shell/windowing/fullscreen.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local M = {}

-- 95% Schwarz (per Theme überschreibbar): "#000000F2"
local DIM_BG = beautiful.dim_overlay_bg or "#000000F2"

-- Primär-Screen niemals dimmen? (auch wenn Fullscreen woanders ist)
local NEVER_DIM_PRIMARY = false

-- ========= Idle-Inhibit =========
local inhibit_pid = nil -- PID von systemd-inhibit sleep
local xset_active = false -- ob wir xset Off/No-DPMS gesetzt haben

local function enable_idle_inhibit()
	-- Falls bereits aktiv: nix tun
	if inhibit_pid or xset_active then
		return
	end

	-- 1) Try systemd-inhibit (startet einen wachhaltenden Prozess)
	local ok, pid = pcall(function()
		-- sleep infinity im Hintergrund halten (wird beim kill beendet)
		return awful.spawn({
			"systemd-inhibit",
			"--what=idle",
			"--who=awesome",
			"--why=fullscreen-active",
			"sleep",
			"infinity",
		})
	end)

	if ok and pid then
		inhibit_pid = pid
		return
	end

	-- 2) Fallback: X11 DPMS/Screensaver deaktivieren
	--  -dpms = DPMS aus, s off = Screensaver aus
	awful.spawn.easy_async_with_shell("xset -dpms; xset s off", function() end)
	xset_active = true
end

local function disable_idle_inhibit()
	-- systemd-inhibit stoppen
	if inhibit_pid then
		pcall(function()
			awful.spawn({ "kill", "-TERM", tostring(inhibit_pid) })
		end)
		inhibit_pid = nil
	end
	-- xset zurücksetzen
	if xset_active then
		-- DPMS/SS wieder einschalten (Standard)
		awful.spawn.easy_async_with_shell("xset +dpms; xset s on", function() end)
		xset_active = false
	end
end

-- ========= Dim-Overlay =========
-- Wir nutzen bewusst die **volle Screen-Geometrie** (Bars werden mit überdeckt).
-- Kein Resizing – nur einmalig beim Erzeugen.
local overlays = {} -- screen -> wibox

local function ensure_overlay(s)
	if overlays[s] and overlays[s].valid then
		return overlays[s]
	end
	local g = s.geometry
	local o = wibox({
		screen = s,
		visible = false,
		ontop = true,
		type = "splash", -- beeinflusst Workarea nicht
		bg = DIM_BG,
		x = g.x,
		y = g.y,
		width = g.width,
		height = g.height,
	})
	o.input_passthrough = true -- Maus/Keys nicht blocken
	overlays[s] = o
	return o
end

local function hide_all()
	for _, o in pairs(overlays) do
		if o and o.valid then
			o.visible = false
		end
	end
end

local function find_fullscreen_screen()
	local fc = client.focus
	if fc and fc.valid and fc.fullscreen then
		return fc.screen
	end
	for c in
		awful.client.iterate(function(x)
			return x and x.valid and x.fullscreen
		end)
	do
		return c.screen
	end
	return nil
end

-- Debounce, um nicht bei jedem Mini-Wechsel inhibit an/aus zu feuern
local pending_update = nil
local function schedule_update(fn)
	if pending_update then
		pending_update:again()
		return
	end
	pending_update = gears.timer({
		timeout = 0.05,
		autostart = true,
		single_shot = true,
		callback = function()
			pending_update = nil
			pcall(fn)
		end,
	})
end

local function do_update()
	local fs_screen = find_fullscreen_screen()

	-- Idle-Inhibit je nach Fullscreen an/aus
	if fs_screen then
		enable_idle_inhibit()
	else
		disable_idle_inhibit()
	end

	-- Dim-Overlays schalten
	if not fs_screen then
		hide_all()
		return
	end
	local primary = awful.screen.primary
	for s, o in pairs(overlays) do
		if o and o.valid then
			local should_dim = (s ~= fs_screen) and not (NEVER_DIM_PRIMARY and s == primary)
			o.visible = should_dim
		end
	end
end

local function update()
	schedule_update(do_update)
end

function M.init()
	-- Overlays einmalig für alle vorhandenen Screens anlegen
	for s in screen do
		ensure_overlay(s)
	end

	-- Reagiere nur auf Zustandswechsel, aber **ohne** Resize
	client.connect_signal("property::fullscreen", update)
	client.connect_signal("focus", update)
	client.connect_signal("unmanage", update)
	tag.connect_signal("property::selected", update)

	-- Neue/entfernte Screens behandeln (ohne Größenanpassung bestehender)
	screen.connect_signal("added", function(s)
		ensure_overlay(s)
		gears.timer.delayed_call(update)
	end)
	screen.connect_signal("removed", function(s)
		local o = overlays[s]
		if o and o.valid then
			o.visible = false
			o:remove()
		end
		overlays[s] = nil
		gears.timer.delayed_call(update)
	end)

	-- Beim Quit sauber aufräumen (Inhibit wieder freigeben)
	awesome.connect_signal("exit", function()
		disable_idle_inhibit()
	end)

	-- Initial anwenden
	gears.timer.delayed_call(update)
end

return M
