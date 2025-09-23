-- ~/.config/awesome/features/shell/menu/dialogs/power/init.lua
-- Theme-Require raus, Base erledigt das Resolving
-- local Theme = require("features.shell.menu.dialogs.parts.theme")
local Layouts = require("features.shell.menu.dialogs.power.layouts")

local M = {}

local function with_theme(layout_fn, name_for_error)
	return function(overrides)
		if type(layout_fn) ~= "function" then
			error(("power module does not export %s"):format(name_for_error or "the requested layout"))
		end
		-- KEIN Theme.get mehr hier – einfach Overrides durchreichen:
		return layout_fn(overrides or {})
	end
end

M.power = with_theme(Layouts.power, "power")
M.logout = with_theme(Layouts.logout, "logout")

-- optional: falls du weiterhin direkten Zugriff brauchst
-- M.theme = require("features.shell.menu.dialogs.parts.theme")

return M
