-- ~/.config/awesome/shell/menu/lib/close_guard.lua
local awful = require("awful")
local gears = require("gears")

local State = require("shell.menu.lib.state")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.arm(on_close)
	local closer = function()
		if type(on_close) == "function" then
			on_close()
		end
	end

	-- ---------------------------------------------------------------------
	-- Root Buttons
	-- ---------------------------------------------------------------------

	State.set_root_buttons(root.buttons())

	local tmp_buttons = gears.table.join(
		State.get_root_buttons() or {},
		awful.button({}, 1, closer),
		awful.button({}, 2, closer),
		awful.button({}, 3, closer)
	)

	root.buttons(tmp_buttons)

	-- ---------------------------------------------------------------------
	-- Client Click
	-- ---------------------------------------------------------------------

	local client_callback = function()
		closer()
	end

	State.set_client_callback(client_callback)
	client.connect_signal("button::press", client_callback)
end

function M.disarm()
	-- ---------------------------------------------------------------------
	-- Root Buttons
	-- ---------------------------------------------------------------------

	local root_buttons = State.get_root_buttons()
	if root_buttons then
		pcall(function()
			root.buttons(root_buttons)
		end)
	end

	State.clear_root_buttons()

	-- ---------------------------------------------------------------------
	-- Client Click
	-- ---------------------------------------------------------------------

	local client_callback = State.get_client_callback()
	if client_callback then
		pcall(function()
			client.disconnect_signal("button::press", client_callback)
		end)
	end

	State.clear_client_callback()
end

return M
