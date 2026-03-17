-- ~/.config/awesome/system/init.lua
local config = require("system.config")
local errors = require("system.errors")

local M = {
	config = config,
	errors = errors,
}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(overrides)
	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	if overrides then
		for key, value in pairs(overrides) do
			M.config[key] = value
		end
	end

	if M.errors and M.errors.hook then
		M.errors.hook()
	end

	local cfg = M.config

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
