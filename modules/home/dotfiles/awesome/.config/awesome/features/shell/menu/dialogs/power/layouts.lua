-- features/shell/menu/dialogs/power/layouts.lua
local awful = require("awful")
local Base = require("features.shell.menu.dialogs.power.base")

local M = {}

-- ===== Helper =====
local function switch_user_and_close(close)
	close()
	local cmd = [[
    if command -v dm-tool >/dev/null 2>&1; then
      dm-tool switch-to-greeter
    elif command -v gdmflexiserver >/dev/null 2>&1; then
      gdmflexiserver
    else
      if command -v notify-send >/dev/null 2>&1; then
        notify-send "Switch user" "Kein passender DM-Befehl (dm-tool/gdmflexiserver) gefunden."
      fi
    fi
  ]]
	awful.spawn.with_shell(cmd)
end

local function awesome_quit(close)
	close()
	awesome.quit()
end

-- ===== Layouts =====
function M.logout_confirm(theme_overrides)
	return Base.choice({
		title = "Log off",
		actions = {
			{ emoji = "👤", label = "Switch user", on_press = switch_user_and_close },
			{ emoji = "🚪", label = "Log off", on_press = awesome_quit },
		},
		theme = theme_overrides,
	})
end

function M.power(theme_overrides)
	return Base.choice({
		title = "Turn off Computer",
		actions = {
			{
				emoji = "🛌",
				label = "Stand By",
				on_press = function(close)
					close()
					awful.spawn.with_shell("systemctl suspend")
				end,
			},
			{
				emoji = "⏻",
				label = "Turn Off",
				on_press = function(close)
					close()
					awful.spawn.with_shell("systemctl poweroff")
				end,
			},
			{
				emoji = "🔄",
				label = "Restart",
				on_press = function(close)
					close()
					awful.spawn.with_shell("systemctl reboot")
				end,
			},
		},
		theme = theme_overrides,
	})
end

return M
