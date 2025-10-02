-- ~/.config/awesome/shell/menu/parts.containers/init.lua
local M = {}

local function load(name)
	if name == "power" or name == nil then
		return require("shell.menu.parts.containers.power")
	elseif name == "panel" then
		return require("shell.menu.parts.containers.panel")
	else
		-- Default auf power
		return require("shell.menu.parts.containers.power")
	end
end

-- Baue den Stack (nur Struktur, Widgets werden angeliefert)
function M.build(kind, th, dims, slots)
	local C = load(kind)
	return C.build(th, dims, slots or {})
end

-- Popup-Wrapper belasse ich wie gehabt (falls du schon einen hast)
M.popup = require("shell.menu.parts.containers.popup")

return M
