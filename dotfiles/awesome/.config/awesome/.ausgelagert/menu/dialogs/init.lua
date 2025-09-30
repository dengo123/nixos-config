-- ~/.config/awesome/features/shell/menu/dialogs/init.lua
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

-- >>> NEU: Control Panel lazy loader
local function load_control_panel()
	if not _mods.control_panel then
		_mods.control_panel = require("features.shell.menu.dialogs.control")
	end
	return _mods.control_panel
end

-- Wrapper
local function call_power_power(overrides)
	return load_power().power(overrides)
end

local function call_power_logout(overrides)
	local P = load_power()
	local f = P.logout or P.logout_confirm
	assert(type(f) == "function", "power module does not export logout/logout_confirm")
	return f(overrides)
end

local function call_hotkeys(overrides)
	return load_hotkeys().hotkeys(overrides)
end

-- >>> NEU: Control Panel Wrapper
local function call_control_panel(overrides)
	return load_control_panel().open(overrides)
end

-- Registry
local registry = {
	power = call_power_power,
	logout = call_power_logout,
	hotkeys = call_hotkeys,
	-- >>> NEU:
	control_panel = call_control_panel,
}

-- Generischer Öffner
function M.open(name, overrides)
	local fn = registry[name]
	assert(fn, ("Unknown dialog: %s"):format(tostring(name)))
	return fn(overrides)
end

-- Direkt-Exports
function M.power(overrides)
	return registry.power(overrides)
end

function M.logout(overrides)
	return registry.logout(overrides)
end

function M.hotkeys(overrides)
	return registry.hotkeys(overrides)
end

-- >>> NEU:
function M.control_panel(overrides)
	return registry.control_panel(overrides)
end

-- Erweiterbar
function M.register(name, ctor)
	assert(type(name) == "string" and name ~= "", "register: name must be non-empty string")
	assert(type(ctor) == "function", "register: ctor must be a function (overrides -> dialog)")
	registry[name] = ctor
end

return M
