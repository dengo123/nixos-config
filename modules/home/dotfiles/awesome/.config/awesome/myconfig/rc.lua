-- ~/.config/awesome/rc.lua
pcall(require, "luarocks.loader")

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local beautiful = require("beautiful")

-- eigene Module
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.config/awesome/?.lua"
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.config/awesome/?/init.lua"

-- Theme
beautiful.init(require("theme"))

-- Keys laden & aktivieren
local keys = require("keys")
keys.apply()

-- Regeln
awful.rules.rules = require("rules")

-- Pro Screen genau EIN Start‑Tag anlegen (nur wenn noch keiner existiert)
screen.connect_signal("request::desktop_decoration", function(s)
	if #s.tags == 0 then
		awful.tag.add("1", {
			screen = s,
			layout = awful.layout.suit.tile,
			selected = true,
		})
	end
end)

-- === Sichtbarkeit nach Start/Reload zuverlässig herstellen ============
local function select_best_tag(s)
	s = s or awful.screen.focused()
	-- 1) Versuche, den letzten Tag aus der Historie wiederherzustellen
	if awful.tag.history.restore(s, 1) then
		return
	end
	-- 2) Falls aktueller Tag leer ist, nimm den Tag mit den meisten Clients
	local cur = s.selected_tag
	local cur_count = (cur and #cur:clients()) or 0
	if cur and cur_count > 0 then
		return
	end
	local best, bestn = nil, 0
	for _, t in ipairs(s.tags or {}) do
		local n = #t:clients()
		if n > bestn then
			best, bestn = t, n
		end
	end
	if best and bestn > 0 then
		best:view_only()
	end
end

-- Nach Startup/Restart mit etwas Verzögerung (Clients müssen remanaged werden)
awesome.connect_signal("startup", function()
	-- leichte Verzögerung, damit alle Clients wieder am Tag hängen
	local t = require("gears").timer({ timeout = 0.4, autostart = true, single_shot = true })
	t:connect_signal("timeout", function()
		for s in screen do
			select_best_tag(s)
		end
	end)
end)

-- Wenn ein Client gemanaged wird und auf einem unsichtbaren Tag landet:
client.connect_signal("manage", function(c)
	local t = c.first_tag or (c.screen and c.screen.selected_tag)
	if t and not t.selected then
		-- nur dann wechseln, wenn der aktuell sichtbare Tag leer ist (verhindert ständiges Springen)
		local s = t.screen
		local cur = s and s.selected_tag
		if cur and #cur:clients() == 0 then
			t:view_only()
		end
	end
end)
-- =====================================================================

-- Wibar + Widgets
require("widgets.wibar")
