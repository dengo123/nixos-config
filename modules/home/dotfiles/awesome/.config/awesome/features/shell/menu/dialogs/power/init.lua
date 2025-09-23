-- ~/.config/awesome/features/shell/menu/dialogs/power/init.lua
local Theme = require("features.shell.menu.dialogs.power.theme")
local Layouts = require("features.shell.menu.dialogs.power.layouts")

local M = {}

local function with_theme(layout_fn, name_for_error)
	return function(overrides)
		if type(layout_fn) ~= "function" then
			error(("power module does not export %s"):format(name_for_error or "the requested layout"))
		end
		local th = Theme.get(overrides or {})
		-- WICHTIG: Layouts.power/logout bauen selbst via Base.choice(...) und öffnen Popup dort.
		return layout_fn(th)
	end
end

M.power = with_theme(Layouts.power, "power")
M.logout = with_theme(Layouts.logout, "logout")
M.theme = Theme

return M
