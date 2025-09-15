local awful = require("awful")
local Base = require("features.shell.menu.shared.dialogs.base")

local M = {}

-- Power-Dialog im XP-Stil
function M.power(theme_overrides)
	local actions = {
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
	}
	return Base.choice({
		title = "Turn off computer",
		actions = actions,
		theme = theme_overrides,
	})
end

return M
