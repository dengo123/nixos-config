-- ~/.config/awesome/features/shell/menu/dialogs/logout.lua
local awful = require("awful")
local Base = require("features.shell.menu.dialogs.parts.base")

local M = {}

-- versucht "Switch User" für verschiedene DMs (LightDM, GDM-Alt)
local function switch_user_and_close(close)
	close()
	local cmd = [[
    if command -v dm-tool >/dev/null 2>&1; then
      dm-tool switch-to-greeter
    elif command -v gdmflexiserver >/dev/null 2>&1; then
      gdmflexiserver
    else
      if command -v notify-send >/dev/null 2>&1; then
        notify-send "Switch user" "Kein passender Display-Manager Befehl gefunden (dm-tool/gdmflexiserver)."
      fi
    fi
  ]]
	awful.spawn.with_shell(cmd)
end

-- Soft Logout: nur Awesome beenden
local function logoff_and_close(close)
	close()
	awesome.quit()
end

function M.logout_confirm(theme_overrides)
	local actions = {
		{
			emoji = "👤",
			label = "Switch user",
			on_press = switch_user_and_close,
		},
		{
			emoji = "🚪",
			label = "Log off",
			on_press = logoff_and_close,
		},
	}

	return Base.choice({
		title = "Log off",
		actions = actions,
		theme = theme_overrides,
	})
end

return M
