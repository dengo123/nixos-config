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

	local ui = require("ui")
	ui.init(cfg)

	-- ---------------------------------------------------------------------
	-- Autostart
	-- ---------------------------------------------------------------------

	local autostart = require("system.autostart")
	autostart.init(cfg)

	-- ---------------------------------------------------------------------
	-- Shell
	-- ---------------------------------------------------------------------

	local input = require("input")
	local shell = require("shell")

	shell.init({
		cfg = cfg,
		ui = ui,
		input = input,
	})

	-- ---------------------------------------------------------------------
	-- Input
	-- ---------------------------------------------------------------------

	input.apply(cfg)

	return cfg
end

return M
