-- ~/.config/awesome/system/init.lua
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

local function build_root_ctx(cfg)
	return {
		cfg = cfg,
		ui = nil,
		input = nil,
		shell = nil,
		system = {},
		modkey = cfg.input and cfg.input.modkey or nil,
	}
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

	local ctx = build_root_ctx(cfg)

	-- ---------------------------------------------------------------------
	-- UI
	-- ---------------------------------------------------------------------

	local UI = require("ui")
	ctx.ui = UI.init({
		cfg = cfg,
	})

	-- ---------------------------------------------------------------------
	-- Autostart
	-- ---------------------------------------------------------------------

	local autostart = require("system.autostart")
	autostart.init(cfg)

	-- ---------------------------------------------------------------------
	-- Shell
	-- ---------------------------------------------------------------------

	local Shell = require("shell")
	ctx.shell = Shell.init({
		cfg = ctx.cfg,
		ui = ctx.ui,
		modkey = ctx.modkey,
	})

	-- ---------------------------------------------------------------------
	-- Input
	-- ---------------------------------------------------------------------

	local Input = require("input")
	ctx.input = Input.init({
		cfg = ctx.cfg,
		ui = ctx.ui,
		shell = ctx.shell,
		modkey = ctx.modkey,
	})

	-- ---------------------------------------------------------------------
	-- Session State
	-- ---------------------------------------------------------------------

	local SessionState = require("system.session_state")
	ctx.system.session_state = SessionState.init({
		cfg = ctx.cfg,
		ui = ctx.ui,
		shell = ctx.shell,
		input = ctx.input,
		system = ctx.system,
		modkey = ctx.modkey,
	})

	if ctx.system.session_state then
		ctx.system.session_state.attach_signals()
		ctx.system.session_state.restore_on_start()
	end

	-- ---------------------------------------------------------------------
	-- Apply Input
	-- ---------------------------------------------------------------------

	if ctx.input and ctx.input.apply then
		ctx.input.apply(cfg)
	end

	return ctx
end

return M
