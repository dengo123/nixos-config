-- ~/.config/awesome/features/shell/menu/dialogs/power/init.lua
local Base = require("features.shell.menu.dialogs.parts.base")
local Icons = require("features.shell.menu.dialogs.power.icons")
local Lib = require("features.shell.menu.lib") -- ← nur lib

local M = {}

local policy = { close = "before" } -- wie früher

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
		title = "Turn off Computer",
		body_builder = function(th, dims, get_close)
			th.header_h, th.footer_h = dims.header_h, dims.footer_h
			local geom = Icons.compute_metrics(th, dims.w, dims.h)
			return Icons.actions_row(build_actions_power(), th, geom, function()
				return get_close()
			end)
		end,
	})
end

function M.logout()
	return Base.dialog({
		title = "Log off",
		body_builder = function(th, dims, get_close)
			th.header_h, th.footer_h = dims.header_h, dims.footer_h
			local geom = Icons.compute_metrics(th, dims.w, dims.h)
			return Icons.actions_row(build_actions_logout(), th, geom, function()
				return get_close()
			end)
		end,
	})
end

return M
