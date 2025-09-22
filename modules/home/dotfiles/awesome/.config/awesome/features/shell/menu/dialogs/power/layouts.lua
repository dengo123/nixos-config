-- features/shell/menu/dialogs/power/layouts.lua
local awful = require("awful")
local Base = require("features.shell.menu.dialogs.power.base")
local Popup = require("features.shell.menu.dialogs.parts.popup") -- <— hinzufügen

local M = {}

-- Immer: erst ALLE Popups schließen (failsafe), dann das aktuelle handle.close(),
-- dann die Aktion starten
local function close_then_cmd(cmd)
	return function(close)
		Popup.close_all() -- failsafe: wirklich alles zu
		if close then
			close()
		end
		if cmd and #cmd > 0 then
			awful.spawn.with_shell(cmd)
		end
	end
end

local function close_then_lua(fn)
	return function(close)
		Popup.close_all()
		if close then
			close()
		end
		if fn then
			pcall(fn)
		end
	end
end

-- ===== Aktionen =====
local switch_user = [[
  if command -v dm-tool >/dev/null 2>&1; then
    dm-tool switch-to-greeter
  elif command -v gdmflexiserver >/dev/null 2>&1; then
    gdmflexiserver
  else
    command -v notify-send >/dev/null 2>&1 && notify-send "Switch user" "Kein passender DM-Befehl (dm-tool/gdmflexiserver) gefunden."
  fi
]]

local function awesome_quit_lua()
	awesome.quit()
end

-- ===== Layouts =====
function M.logout(th)
	return Base.choice({
		title = "Log off",
		actions = {
			{ emoji = "👤", label = "Switch user", on_press = close_then_cmd(switch_user) },
			{ emoji = "🚪", label = "Log off", on_press = close_then_lua(awesome_quit_lua) },
		},
		theme = th,
	})
end

function M.power(th)
	return Base.choice({
		title = "Turn off Computer",
		actions = {
			{ emoji = "🛌", label = "Stand By", on_press = close_then_cmd("systemctl suspend") },
			{ emoji = "⏻", label = "Turn Off", on_press = close_then_cmd("systemctl poweroff") },
			{ emoji = "🔄", label = "Restart", on_press = close_then_cmd("systemctl reboot") },
		},
		theme = th,
	})
end

return M
