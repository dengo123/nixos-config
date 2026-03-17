-- ~/.config/awesome/shell/menu/lib/keygrab.lua
local awful = require("awful")

local State = require("shell.menu.lib.state")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.start(on_escape)
	if State.get_keygrab() then
		return
	end

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
	local keygrab = State.get_keygrab()

	if keygrab then
		pcall(function()
			awful.keygrabber.stop(keygrab)
		end)
	end

	State.clear_keygrab()
end

return M
