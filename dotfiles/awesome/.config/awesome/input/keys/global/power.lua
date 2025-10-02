-- ~/.config/awesome/input/keys/global/power.lua
local awful = require("awful")
local gears = require("gears")

return function(modkey, launchers) -- << Injection!
	local function bind(mods, key, fn, desc)
		return awful.key(mods, key, fn, { description = desc, group = "power" })
	end

	local MODS = { {}, { "Mod4" } }

	local function install_modal_keys(handle)
		local old = root.keys()
		local extra = {}

		local function add_allmods(key, fn, desc)
			for _, m in ipairs(MODS) do
				table.insert(extra, awful.key(m, key, fn, { description = desc or key, group = "power" }))
			end
		end
		local function run(cmd)
			awful.spawn.with_shell(cmd)
		end
		local function close()
			if handle and handle.close then
				handle.close()
			end
		end

		add_allmods("u", function()
			close()
			run("systemctl suspend")
		end, "Suspend")
		add_allmods("h", function()
			close()
			run("systemctl hibernate")
		end, "Hibernate")
		add_allmods("r", function()
			close()
			run("systemctl reboot")
		end, "Reboot")
		add_allmods("p", function()
			close()
			run("systemctl poweroff")
		end, "Poweroff")
		add_allmods("l", function()
			close()
			run([[ if command -v loginctl >/dev/null 2>&1; then
               loginctl terminate-user "$USER"
             else
               pkill -KILL -u "$USER"
             fi ]])
		end, "Logout")
		add_allmods("s", function()
			close()
			run([[ if command -v dm-tool >/dev/null 2>&1; then
               dm-tool switch-to-greeter
             elif command -v gdmflexiserver >/dev/null 2>&1; then
               gdmflexiserver
             else
               command -v notify-send >/dev/null 2>&1 && notify-send "Switch user" "Kein passender DM-Befehl gefunden."
             fi ]])
		end, "Switch user")
		add_allmods("Escape", function()
			close()
		end, "Cancel")

		local joined = old and gears.table.join(old, table.unpack(extra)) or gears.table.join(table.unpack(extra))
		root.keys(joined)

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

	return gears.table.join(bind({ modkey }, "Escape", function()
		local handle = launchers.open.power({})
		if handle then
			install_modal_keys(handle)
		end
	end, "open power dialog (modal hotkeys)"))
end
