-- features/shell/menu/dialogs/power/layouts.lua
local Base = require("features.shell.menu.dialogs.parts.base")
local Actions = require("features.shell.menu.dialogs.parts.actions")

local M = {}

-- ===== Aktionen (Shell/Lua) ===============================================

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

-- ===== Layouts ============================================================

function M.logout(th)
	return Base.choice({
		title = "Log off",
		actions = {
			{ emoji = "👤", label = "Switch user", on_press = Actions.cmd(switch_user) },
			{ emoji = "🚪", label = "Log off", on_press = Actions.lua(awesome_quit_lua) },
		},
		theme = th,
	})
end

function M.power(th)
	return Base.choice({
		title = "Turn off Computer",
		actions = {
			{ emoji = "🛌", label = "Stand By", on_press = Actions.cmd("systemctl suspend") },
			{ emoji = "⏻", label = "Turn Off", on_press = Actions.cmd("systemctl poweroff") },
			{ emoji = "🔄", label = "Restart", on_press = Actions.cmd("systemctl reboot") },
		},
		theme = th,
	})
end

return M
