-- ~/.config/awesome/input/client/mouse.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime_api = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function current_api(args)
	return (args and args.api) or runtime_api
end

local function activate_client(c, context, raise)
	if not (c and c.valid) then
		return
	end

	c:emit_signal("request::activate", context or "mouse_click", {
		raise = (raise ~= false),
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime_api = args.api or args or {}
	return M
end

function M.client_buttons(modkey)
	return gears.table.join(
		awful.button({}, 1, function(c)
			activate_client(c, "mouse_click", true)
		end),
		awful.button({ modkey }, 1, function(c)
			activate_client(c, "mouse_click", true)
			awful.mouse.client.move(c)
		end),
		awful.button({ modkey }, 3, function(c)
			activate_client(c, "mouse_click", true)
			awful.mouse.client.resize(c)
		end)
	)
end

function M.titlebar_buttons(c, activate_fn)
	local _api = current_api()
	local activate = activate_fn or activate_client

	return gears.table.join(
		awful.button({}, 1, function()
			activate(c, "titlebar_drag", true)
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			activate(c, "titlebar_resize", true)
			awful.mouse.client.resize(c)
		end)
	)
end

return M
