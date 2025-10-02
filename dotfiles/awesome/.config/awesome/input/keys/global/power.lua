-- ~/.config/awesome/input/keys/global/power.lua
local awful = require("awful")
local gears = require("gears")

return function(modkey)
	local function bind(mods, key, fn, desc)
		return awful.key(mods, key, fn, { description = desc, group = "power" })
	end

	-- erlaubt nackte Tasten + gehaltenes Mod4 (falls Mod4 nach dem Öffnen noch gedrückt ist)
	local MODS = { {}, { "Mod4" } }

	-- Hilfsfunktion: temporäre (modale) Hotkeys solange der Power-Dialog offen ist
	local function install_power_modal_keys(handle)
		local old = root.keys()
		local extra = {}

		local function add(mods, key, fn, desc)
			table.insert(extra, awful.key(mods, key, fn, { description = desc or key, group = "power" }))
		end

		local function add_allmods(key, fn, desc)
			for _, m in ipairs(MODS) do
				add(m, key, fn, desc)
			end
		end

		-- Aktionen
		local function run(cmd)
			awful.spawn.with_shell(cmd)
		end

		local function close_dialog()
			if handle and handle.close then
				handle.close()
			end
		end

		-- Map: u,h,r,p,l,s  (+ Esc = Abbruch)
		add_allmods("u", function()
			close_dialog()
			run("systemctl suspend")
		end, "Suspend")
		add_allmods("h", function()
			close_dialog()
			run("systemctl hibernate")
		end, "Hibernate")
		add_allmods("r", function()
			close_dialog()
			run("systemctl reboot")
		end, "Reboot")
		add_allmods("p", function()
			close_dialog()
			run("systemctl poweroff")
		end, "Poweroff")

		-- Logout: beende Benutzer-Session (fallbacks schonend)
		add_allmods("l", function()
			close_dialog()
			run([[ if command -v loginctl >/dev/null 2>&1; then
               loginctl terminate-user "$USER"
             else
               pkill -KILL -u "$USER"
             fi ]])
		end, "Logout")

		-- Switch User: zum Greeter (wie in deinem Power-Dialog)
		add_allmods("s", function()
			close_dialog()
			run([[ if command -v dm-tool >/dev/null 2>&1; then
               dm-tool switch-to-greeter
             elif command -v gdmflexiserver >/dev/null 2>&1; then
               gdmflexiserver
             else
               command -v notify-send >/dev/null 2>&1 && notify-send "Switch user" "Kein passender DM-Befehl gefunden."
             fi ]])
		end, "Switch user")

		-- Esc: nur schließen
		add_allmods("Escape", function()
			close_dialog()
		end, "Cancel")

		-- aktivieren (anhängen, nicht ersetzen)
		local joined = old and gears.table.join(old, table.unpack(extra)) or gears.table.join(table.unpack(extra))
		root.keys(joined)

		-- beim Close zurückrollen
		local oc = handle.close
		handle.close = function(...)
			if old then
				root.keys(old)
			else
				root.keys(nil)
			end
			return oc(...)
		end
	end

	-- Mod4+Esc → Power-Dialog öffnen & modale Hotkeys installieren
	return gears.table.join(bind({ modkey }, "Escape", function()
		local Power = require("shell.menu.power")
		local handle = Power.open()
		if handle then
			install_power_modal_keys(handle)
		end
	end, "open power dialog (with modal hotkeys)"))
end
