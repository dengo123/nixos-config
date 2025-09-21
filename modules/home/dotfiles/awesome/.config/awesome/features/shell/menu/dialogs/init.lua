-- features/shell/menu/dialogs/init.lua
-- Zentrale Registry, die die Sub-Module lädt

local M = {}

-- Subsysteme lazy-laden
local function require_power()
	return require("features.shell.menu.dialogs.power.init")
end

local function require_generic()
	return require("features.shell.menu.dialogs.generic.init")
end

-- Standard-Registry
local registry = {
	power = function(overrides)
		return require_power().power(overrides)
	end,
	logout = function(overrides)
		return require_power().logout_confirm(overrides)
	end,
	-- weitere generische Dialoge können hier hängen:
	-- example_generic = function(overrides) return require_generic().example(overrides) end,
}

function M.open(name, overrides)
	local fn = registry[name]
	assert(fn, ("Unknown dialog: %s"):format(tostring(name)))
	return fn(overrides)
end

-- Backwards-kompatible Direkt-Exports
function M.power(overrides)
	return registry.power(overrides)
end

function M.logout_confirm(overrides)
	return registry.logout(overrides)
end

-- Erweiterbar von außen
function M.register(name, ctor)
	assert(type(name) == "string" and name ~= "", "register: name must be non-empty string")
	assert(type(ctor) == "function", "register: ctor must be function (theme_overrides -> popup)")
	registry[name] = ctor
end

return M
