-- ~/.config/awesome/system/init.lua
local gears = require("gears")

local config = require("system.config")
local errors = require("system.errors")

local M = {
	config = config,
	errors = errors,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function deep_copy(value)
	if type(value) ~= "table" then
		return value
	end

	local out = {}

	for k, v in pairs(value) do
		out[k] = deep_copy(v)
	end

	return out
end

local function deep_merge(dst, src)
	for k, v in pairs(src or {}) do
		if type(v) == "table" and type(dst[k]) == "table" then
			deep_merge(dst[k], v)
		else
			dst[k] = deep_copy(v)
		end
	end

	return dst
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(overrides)
	local cfg = deep_copy(M.config)

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	if overrides then
		deep_merge(cfg, overrides)
	end

	if M.errors and M.errors.hook then
		M.errors.hook()
	end

	-- ---------------------------------------------------------------------
	-- UI
	-- ---------------------------------------------------------------------

	local UI = require("ui")
	local ui_mod = UI.init({
		cfg = cfg,
	})
	local ui = ui_mod.get()

	-- ---------------------------------------------------------------------
	-- Autostart
	-- ---------------------------------------------------------------------

	local autostart = require("system.autostart")
	autostart.init(cfg)

	-- ---------------------------------------------------------------------
	-- Input
	-- ---------------------------------------------------------------------

	local input = require("input")
	input = input.init({
		ui = ui,
	})

	-- ---------------------------------------------------------------------
	-- Shell
	-- ---------------------------------------------------------------------

	local shell = require("shell")

	shell.init({
		cfg = cfg,
		ui = ui,
		input = input,
	})

	-- ---------------------------------------------------------------------
	-- Session State
	-- ---------------------------------------------------------------------

	local SessionState = require("system.session_state")
	SessionState = SessionState.init({
		cfg = cfg,
		ui = ui,
	})

	SessionState.attach_signals()
	SessionState.restore_on_start()

	-- ---------------------------------------------------------------------
	-- Apply Input
	-- ---------------------------------------------------------------------

	input.apply(cfg)

	return cfg
end

return M
