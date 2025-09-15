local Base = require("features.shell.menu.dialogs.base")

local M = {}

-- Soft Logout: nur Awesome beenden
function M.logout_confirm(theme_overrides)
	local actions = {
		{
			emoji = "🚪",
			label = "Log off",
			on_press = function(close)
				close()
				awesome.quit()
			end,
		},
	}
	return Base.choice({
		title = "Log off",
		actions = actions,
		theme = theme_overrides,
	})
end

return M
