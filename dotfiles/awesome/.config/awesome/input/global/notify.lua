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

local function center_keys(cfg)
	cfg = cfg or {}

	local input_cfg = cfg.input or {}
	local enabled = input_cfg.notify_center_navigation_keys

	if enabled == nil then
		enabled = true
	end

	if enabled == false then
		return {}
	end

	return gears.table.join(
		awful.key({}, "Escape", function()
			awesome.emit_signal("notify::close_center")
		end, {
			description = "close notify center",
			group = "Notify",
		}),

		awful.key({}, "Up", function()
			awesome.emit_signal("notify::center_select_prev")
		end, {
			description = "notify center select previous",
			group = "Notify",
		}),

		awful.key({}, "Down", function()
			awesome.emit_signal("notify::center_select_next")
		end, {
			description = "notify center select next",
			group = "Notify",
		}),

		awful.key({}, "Prior", function()
			awesome.emit_signal("notify::center_scroll_up")
		end, {
			description = "notify center page up",
			group = "Notify",
		}),

		awful.key({}, "Next", function()
			awesome.emit_signal("notify::center_scroll_down")
		end, {
			description = "notify center page down",
			group = "Notify",
		}),

		awful.key({}, "Return", function()
			awesome.emit_signal("notify::center_activate_selected")
		end, {
			description = "activate selected notification",
			group = "Notify",
		}),

		awful.key({}, "BackSpace", function()
			awesome.emit_signal("notify::center_dismiss_selected")
		end, {
			description = "dismiss selected notification",
			group = "Notify",
		}),

		awful.key({ "Control" }, "Delete", function()
			awesome.emit_signal("notify::center_clear_history")
		end, {
			description = "clear notification history",
			group = "Notify",
		})
	)
end

function M.build(modkey, cfg)
	return gears.table.join(toggle_key(modkey, cfg), center_keys(cfg))
end

return function(modkey, cfg)
	return M.build(modkey, cfg)
end
