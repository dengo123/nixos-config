-- ~/.config/awesome/shell/notify/center/close_guard.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local armed = false
local saved_root_buttons = nil
local client_callback = nil
local esc_grabber = nil

-- =========================================================================
-- Public API
-- =========================================================================

function M.arm(on_close)
	if armed then
		return
	end

	armed = true

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

	saved_root_buttons = root.buttons()

	root.buttons(
		gears.table.join(
			saved_root_buttons or {},
			awful.button({}, 1, closer),
			awful.button({}, 2, closer),
			awful.button({}, 3, closer)
		)
	)

	-- ---------------------------------------------------------------------
	-- Client Click
	-- ---------------------------------------------------------------------

	client_callback = function()
		closer()
	end

	client.connect_signal("button::press", client_callback)

	-- ---------------------------------------------------------------------
	-- Escape
	-- ---------------------------------------------------------------------

	esc_grabber = awful.keygrabber({
		stop_event = "release",
		keypressed_callback = function(_, _, key)
			if key == "Escape" then
				closer()
			end
		end,
	})

	esc_grabber:start()
end

function M.disarm()
	if not armed then
		return
	end

	armed = false

	-- ---------------------------------------------------------------------
	-- Root Buttons
	-- ---------------------------------------------------------------------

	if saved_root_buttons then
		pcall(function()
			root.buttons(saved_root_buttons)
		end)
	end

	saved_root_buttons = nil

	-- ---------------------------------------------------------------------
	-- Client Click
	-- ---------------------------------------------------------------------

	if client_callback then
		pcall(function()
			client.disconnect_signal("button::press", client_callback)
		end)
	end

	client_callback = nil

	-- ---------------------------------------------------------------------
	-- Escape
	-- ---------------------------------------------------------------------

	if esc_grabber then
		pcall(function()
			esc_grabber:stop()
		end)
	end

	esc_grabber = nil
end

return M
