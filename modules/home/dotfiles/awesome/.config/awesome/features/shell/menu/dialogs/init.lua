-- features/shell/menu/dialogs/init.lua
local M = {}

-- Lazy-Loader für eingebaute Dialoge
local function open_power(overrides)
	local mod = require("features.shell.menu.dialogs.power")
	return mod.power(overrides)
end

local function open_logout(overrides)
	local mod = require("features.shell.menu.dialogs.logout")
	return mod.logout_confirm(overrides)
end

local registry = {
	power = open_power,
	logout = open_logout,
}

function M.open(name, overrides)
	local fn = registry[name]
	assert(fn, ("Unknown dialog: %s"):format(tostring(name)))
	return fn(overrides)
end

-- Direkt-Exports (backwards-kompatibel)
function M.power(overrides)
	return open_power(overrides)
end

function M.logout_confirm(overrides)
	return open_logout(overrides)
end

-- Custom-Dialoge von außen registrieren
function M.register(name, ctor)
	assert(type(name) == "string" and name ~= "", "register: name must be non-empty string")
	assert(type(ctor) == "function", "register: ctor must be function (theme_overrides -> popup)")
	registry[name] = ctor
end

return M
