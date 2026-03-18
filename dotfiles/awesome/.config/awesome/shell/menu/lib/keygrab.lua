-- ~/.config/awesome/shell/menu/lib/keygrab.lua
local awful = require("awful")

local State = require("shell.menu.lib.state")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.start(on_escape)
	-- ---------------------------------------------------------------------
	-- Guard
	-- ---------------------------------------------------------------------

	if State.get_keygrab() then
		return
	end

	-- ---------------------------------------------------------------------
	-- Keygrabber
	-- ---------------------------------------------------------------------

	local keygrab = awful.keygrabber.run(function(_, key, event)
		if event == "press" and key == "Escape" then
			if type(on_escape) == "function" then
				on_escape()
			end

			return false
		end

		return true
	end)

	State.set_keygrab(keygrab)
end

function M.stop()
	-- ---------------------------------------------------------------------
	-- State
	-- ---------------------------------------------------------------------

	local keygrab = State.get_keygrab()

	-- ---------------------------------------------------------------------
	-- Stop
	-- ---------------------------------------------------------------------

	if keygrab then
		pcall(function()
			awful.keygrabber.stop(keygrab)
		end)
	end

	State.clear_keygrab()
end

return M
