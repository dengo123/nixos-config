-- ~/.config/awesome/input/global/tags.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function kbd_intent(ms)
	awesome.emit_signal("focus_policy::keyboard_intent", ms or 250)
end

local function move_current_tag_in_list(delta)
	local s = awful.screen.focused()
	if not s then
		return
	end

	local t = s.selected_tag
	if not t then
		return
	end

	local idx = t.index or 1
	local all = s.tags or {}
	local target = idx + delta

	if target < 1 or target > #all then
		return
	end

	awful.tag.move(target, t)
	awful.tag.viewonly(t)
end

local function has_tag_actions(actions)
	return type(actions) == "table"
		and (
			type(actions.view_tag_idx) == "function"
			or type(actions.move_tag_to_screen) == "function"
			or type(actions.add) == "function"
			or type(actions.add_silent) == "function"
			or type(actions.delete_current) == "function"
			or type(actions.delete_current_force) == "function"
		)
end

local function call(actions, name, ...)
	if not (actions and type(actions[name]) == "function") then
		return
	end

	kbd_intent()
	actions[name](...)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	if not has_tag_actions(actions) then
		return gears.table.join()
	end

	return gears.table.join(
		awful.key({ modkey, "Control" }, "Right", function()
			call(actions, "view_tag_idx", 1)
		end, {
			description = "View Next Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Control" }, "Left", function()
			call(actions, "view_tag_idx", -1)
		end, {
			description = "View Previous Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Control" }, "Up", function()
			kbd_intent()
			move_current_tag_in_list(-1)
		end, {
			description = "Move Current Tag Up",
			group = "Tags",
		}),

		awful.key({ modkey, "Control" }, "Down", function()
			kbd_intent()
			move_current_tag_in_list(1)
		end, {
			description = "Move Current Tag Down",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Left", function()
			call(actions, "move_tag_to_screen", "left")
		end, {
			description = "Move Tag To Screen Left",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Right", function()
			call(actions, "move_tag_to_screen", "right")
		end, {
			description = "Move Tag To Screen Right",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Up", function()
			call(actions, "move_tag_to_screen", "up")
		end, {
			description = "Move Tag To Screen Up",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Down", function()
			call(actions, "move_tag_to_screen", "down")
		end, {
			description = "Move Tag To Screen Down",
			group = "Tags",
		}),

		awful.key({ modkey }, "n", function()
			call(actions, "add")
		end, {
			description = "Create New Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Shift" }, "n", function()
			call(actions, "add_silent")
		end, {
			description = "Create New Tag Silently",
			group = "Tags",
		}),

		awful.key({ modkey }, "c", function()
			call(actions, "delete_current")
		end, {
			description = "Close Current Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Shift" }, "c", function()
			call(actions, "delete_current_force")
		end, {
			description = "Force Close Current Tag",
			group = "Tags",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
