-- containers/init.lua
local M = {}

-- Container-Registry
function M.build(kind, th, dims, widgets)
	if kind == "panel" then
		return require("features.shell.menu.dialogs.parts.containers.panel").build(th, dims, widgets)
	elseif kind == "power" then
		return require("features.shell.menu.dialogs.parts.containers.power").build(th, dims, widgets)
	else
		-- Fallback: power
		return require("features.shell.menu.dialogs.parts.containers.power").build(th, dims, widgets)
	end
end

-- Popup unter containers/ kapseln & re-exportieren
M.popup = require("features.shell.menu.dialogs.parts.containers.popup")
-- -> M.popup.show(...), M.popup.close_all(), ...

return M
