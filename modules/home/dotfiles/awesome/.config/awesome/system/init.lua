-- ~/.config/awesome/system/init.lua
local config = require("system.config")
local errors = require("system.errors")

local M = {
	config = config, -- deine bestehende Tabelle aus system/config.lua
	errors = errors, -- dein bestehendes Modul aus system/errors.lua
}

--- Optionaler Convenience-Einstieg:
--- Wendet Fehler-Hooks an und erlaubt optionale Overrides für config.
--- Gibt immer die finale config zurück.
--- @param overrides table|nil
function M.init(overrides)
	if overrides then
		for k, v in pairs(overrides) do
			M.config[k] = v
		end
	end
	if M.errors and M.errors.hook then
		M.errors.hook()
	end
	return M.config
end

return M
