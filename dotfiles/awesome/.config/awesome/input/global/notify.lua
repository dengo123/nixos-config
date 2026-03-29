-- ~/.config/awesome/input/global/notify.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local function toggle_key(modkey, cfg)
	cfg = cfg or {}

	local input_cfg = cfg.input or {}
	local key = input_cfg.notify_center_key

	if key == nil then
		key = "Insert"
	end

	if key == false or key == "" then
		return {}
	end

	return awful.key({ modkey }, key, function()
		awesome.emit_signal("notify::toggle_center")
	end, {
		description = "toggle notify center",
		group = "Notify",
	})
end

function M.build(modkey, cfg)
	return gears.table.join(toggle_key(modkey, cfg))
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
