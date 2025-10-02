-- ~/.config/awesome/shell/launchers/init.lua
local L = {}

local function safe_require(p)
	local ok, mod = pcall(require, p)
	return ok and mod or nil
end

-- gemeinsame Infra
L.lib = safe_require("shell.launchers.lib") -- { actions, cancel, popup, attach/inject (falls gewünscht) }

-- konkrete Launcher
L.power = safe_require("shell.launchers.power") -- erwartet .open(opts, lib)
L.run = safe_require("shell.launchers.run") -- erwartet .open(opts, lib)

-- Bequeme Direktaufrufe mit auto-lib
L.open = {
	power = function(opts)
		return L.power and L.power.open(opts, L.lib)
	end,
	run = function(opts)
		return L.run and L.run.open(opts, L.lib)
	end,
}

return L
