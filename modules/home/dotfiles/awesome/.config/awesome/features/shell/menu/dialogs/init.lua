-- ~/.config/awesome/features/shell/menu/dialogs/init.lua
-- Zentrale Registry mit Lazy-Loading und klaren Namen (power, logout, hotkeys).
-- "logout_confirm" bleibt nur als kompatibler Wrapper auf "logout".

local M = {}

-- Lazy-Cache für Submodule
local _mods = {}

local function load_power()
	if not _mods.power then
		_mods.power = require("features.shell.menu.dialogs.power")
	end
	return _mods.power
end

local function load_hotkeys()
	if not _mods.hotkeys then
		_mods.hotkeys = require("features.shell.menu.dialogs.hotkeys")
	end
	return _mods.hotkeys
end

-- Hilfsfunktionen, die tolerant sind, falls das Power-Modul (noch) kein .logout exportiert
local function call_power_power(overrides)
	return load_power().power(overrides)
end

local function call_power_logout(overrides)
	local P = load_power()
	-- Option A: bevorzugt .logout; fallback auf .logout_confirm für Back-Compat
	local f = P.logout or P.logout_confirm
	assert(type(f) == "function", "power module does not export logout/logout_confirm")
	return f(overrides)
end

-- Registry
local registry = {
	power = call_power_power,
	logout = call_power_logout,
	hotkeys = function(overrides)
		return load_hotkeys().hotkeys(overrides)
	end,
}

-- Generischer Öffner per Name
function M.open(name, overrides)
	local fn = registry[name]
	assert(fn, ("Unknown dialog: %s"):format(tostring(name)))
	return fn(overrides)
end

-- Sprechende Direkt-Exports
function M.power(overrides)
	return registry.power(overrides)
end

function M.logout(overrides)
	return registry.logout(overrides)
end

-- Kompat-Wrapper: weiterhin aufrufbar, leitet auf "logout" um
function M.logout_confirm(overrides)
	return M.logout(overrides)
end

function M.hotkeys(overrides)
	return registry.hotkeys(overrides)
end

-- Erweiterbar von außen: neuen Dialognamen registrieren
-- ctor: function(overrides) -> popup_handle | nil
function M.register(name, ctor)
	assert(type(name) == "string" and name ~= "", "register: name must be non-empty string")
	assert(type(ctor) == "function", "register: ctor must be a function (overrides -> dialog)")
	registry[name] = ctor
end

return M
