-- ~/.config/awesome/input/runtime/notify.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local notify_keys = nil
local base_globalkeys = nil
local active = false

-- =========================================================================
-- Internal
-- =========================================================================

local function center_is_open(overlays)
	local ordered = (overlays and overlays.ordered) or {}

	for _, overlay in ipairs(ordered) do
		if overlay and overlay.name == "notify_center" then
			local is_open = overlay.is_open
			return type(is_open) == "function" and is_open() or false
		end
	end

	return false
end

local function build_notify_keys()
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

local function install(root_keys)
	if active then
		return
	end

	active = true
	root.keys(gears.table.join(root_keys, notify_keys))
end

local function uninstall(root_keys)
	if not active then
		return
	end

	active = false
	root.keys(root_keys)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	base_globalkeys = args.globalkeys
	notify_keys = build_notify_keys()

	awesome.connect_signal("ui::overlays_changed", function()
		if center_is_open(args.overlays) then
			install(base_globalkeys)
		else
			uninstall(base_globalkeys)
		end
	end)

	if center_is_open(args.overlays) then
		install(base_globalkeys)
	else
		uninstall(base_globalkeys)
	end
end

return M
