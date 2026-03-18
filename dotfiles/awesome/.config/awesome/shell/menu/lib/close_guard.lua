-- ~/.config/awesome/shell/menu/lib/close_guard.lua
local awful = require("awful")
local gears = require("gears")

local State = require("shell.menu.lib.state")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.arm(on_close)
	-- ---------------------------------------------------------------------
	-- Helpers
	-- ---------------------------------------------------------------------

	local function closer()
		if type(on_close) == "function" then
			on_close()
		end
	end

	-- ---------------------------------------------------------------------
	-- Root Buttons
	-- ---------------------------------------------------------------------

	State.set_root_buttons(root.buttons())

	local tmp = gears.table.join(
		State.get_root_buttons() or {},
		awful.button({}, 1, closer),
		awful.button({}, 2, closer),
		awful.button({}, 3, closer)
	)

	root.buttons(tmp)

	-- ---------------------------------------------------------------------
	-- Client Click
	-- ---------------------------------------------------------------------

	local client_cb = function(_c, _x, _y, _button)
		closer()
	end

	State.set_client_callback(client_cb)
	client.connect_signal("button::press", client_cb)
end

function M.disarm()
	-- ---------------------------------------------------------------------
	-- Root Buttons
	-- ---------------------------------------------------------------------

	local root_btns = State.get_root_buttons()
	if root_btns then
		pcall(function()
			root.buttons(root_btns)
		end)
	end

	State.clear_root_buttons()

	-- ---------------------------------------------------------------------
	-- Client Click
	-- ---------------------------------------------------------------------

	local client_cb = State.get_client_callback()
	if client_cb then
		pcall(function()
			client.disconnect_signal("button::press", client_cb)
		end)
	end

	State.clear_client_callback()
end

return M
