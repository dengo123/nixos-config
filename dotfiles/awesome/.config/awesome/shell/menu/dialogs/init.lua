-- ~/.config/awesome/shell/menu/dialogs/init.lua
local awful = require("awful")

local M = {}

-- Kontext (für Zugriff auf cfg/ui, z. B. Launcher-Cmd)
local _ctx = { ui = nil, cfg = nil }

-- Lazy-Cache für Submodule
local _mods = {}

local function _safe_require(path)
	local ok, mod = pcall(require, path)
	if not ok then
		error(("menu.dialogs: cannot require '%s': %s"):format(path, tostring(mod)), 2)
	end
	return mod
end

-- Submodule (neue Pfade ohne 'features.')
local function load_power()
	if not _mods.power then
		_mods.power = _safe_require("shell.menu.dialogs.power")
	end
	return _mods.power
end

-- ---------------------------------------------------------------------------
-- Öffentliche Initialisierung (Kompatibel zur neuen cfg/ui-Weitergabe)

function M.init(args)
	args = args or {}
	_ctx.ui = args.ui or {}
	_ctx.cfg = args.cfg or {}
	return M
end

-- ---------------------------------------------------------------------------
-- Wrapper / Shims

-- Zentral: Power-Dialog (ersetzt frühere getrennte power/logout Ctors)
local function call_power(overrides)
	local P = load_power()
	assert(type(P.power) == "function", "power module must export .power(overrides)")
	return P.power(overrides)
end

-- Abwärtskompatibles Logout:
-- bevorzugt P.power({ mode='logout' | action='logout' }), fallback auf P.logout / P.logout_confirm
local function call_logout(overrides)
	overrides = overrides or {}
	local P = load_power()

	-- 1) Versuche den neuen Weg über P.power(...)
	if type(P.power) == "function" then
		local o = {}
		for k, v in pairs(overrides) do
			o[k] = v
		end
		-- markiere klar den Logout-Fall; Power-Dialog kann das interpretieren
		o.mode = o.mode or "logout"
		o.action = o.action or "logout"
		return P.power(o)
	end

	-- 2) Fallbacks für alte Impl.
	local f = P.logout or P.logout_confirm
	assert(type(f) == "function", "power module does not export power/logout")
	return f(overrides)
end

-- Neuer Launcher-Eintrag (ersetzt „control panel“)
local function call_launcher(overrides)
	overrides = overrides or {}
	local cmd = overrides.command

	if not cmd or #tostring(cmd) == 0 then
		-- cfg.launcher bevorzugen, sonst Default
		local cfg_cmd = (_ctx.cfg and type(_ctx.cfg.launcher) == "string" and #_ctx.cfg.launcher > 0)
				and _ctx.cfg.launcher
			or nil

		if cfg_cmd and cfg_cmd:lower() == "rofi" then
			cmd = "rofi -show drun"
		elseif cfg_cmd and cfg_cmd:lower() ~= "launcher" then
			cmd = cfg_cmd
		else
			cmd = "rofi -show drun"
		end
	end

	awful.spawn.with_shell(cmd)
	return true
end

-- ---------------------------------------------------------------------------
-- Registry

local registry = {
	power = call_power,
	logout = call_logout, -- Alias (Abwärtskompatibilität)
	launcher = call_launcher, -- ersetzt „control panel“
}

function M.open(name, overrides)
	local fn = registry[name]
	assert(fn, ("menu.dialogs: unknown dialog '%s'"):format(tostring(name)))
	return fn(overrides)
end

-- Direkte Shortcuts (optional)
function M.power(overrides)
	return registry.power(overrides)
end

function M.logout(overrides)
	return registry.logout(overrides)
end

function M.launcher(overrides)
	return registry.launcher(overrides)
end

-- Erweiterbar
function M.register(name, ctor)
	assert(type(name) == "string" and name ~= "", "register: name must be non-empty string")
	assert(type(ctor) == "function", "register: ctor must be a function (overrides -> dialog)")
	registry[name] = ctor
end

return M
