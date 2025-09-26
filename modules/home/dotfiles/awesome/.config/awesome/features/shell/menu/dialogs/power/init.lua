-- ~/.config/awesome/features/shell/menu/dialogs/power/init.lua
local Base = require("features.shell.menu.dialogs.parts")
local Lib = require("features.shell.menu.lib")

local M = {}

local policy = { close = "before" }

local switch_user = [[
  if command -v dm-tool >/dev/null 2>&1; then
    dm-tool switch-to-greeter
  elif command -v gdmflexiserver >/dev/null 2>&1; then
    gdmflexiserver
  else
    command -v notify-send >/dev/null 2>&1 && notify-send "Switch user" "Kein passender DM-Befehl gefunden."
  fi
]]

local function awesome_quit_lua()
	awesome.quit()
end

local function build_actions_power()
	return {
		{ emoji = "🛌", label = "Stand By", on_press = Lib.cmd("systemctl suspend", policy) },
		{ emoji = "⏻", label = "Turn Off", on_press = Lib.cmd("systemctl poweroff", policy) },
		{ emoji = "🔄", label = "Restart", on_press = Lib.cmd("systemctl reboot", policy) },
	}
end

local function build_actions_logout()
	return {
		{ emoji = "👤", label = "Switch user", on_press = Lib.cmd(switch_user, policy) },
		{ emoji = "🚪", label = "Log off", on_press = Lib.lua(awesome_quit_lua, policy) },
	}
end

function M.power()
	return Base.dialog({
		container = "power", -- optional; ist default
		title = "Turn off Computer",
		body_builder = function(th, dims, get_close)
			return Base.icons_row(build_actions_power(), th, dims, function()
				return get_close()
			end)
		end,
	})
end

function M.logout()
	return Base.dialog({
		container = "power",
		title = "Log off",
		body_builder = function(th, dims, get_close)
			return Base.icons_row(build_actions_logout(), th, dims, function()
				return get_close()
			end)
		end,
	})
end

return M
