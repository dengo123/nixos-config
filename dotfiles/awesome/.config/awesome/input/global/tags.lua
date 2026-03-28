-- ~/.config/awesome/input/global/tags.lua
local awful = require("awful")

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

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, actions)
	-- assert(type(actions) == "table", "input.global.tags: actions fehlt/ungueltig")

	return awful.util.table.join(
		awful.key({ modkey, "Control" }, "Right", function()
			kbd_intent()
			actions.view_tag_idx(1)
		end, {
			description = "View Next Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Control" }, "Left", function()
			kbd_intent()
			actions.view_tag_idx(-1)
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
			kbd_intent()
			actions.move_tag_to_screen("left")
		end, {
			description = "Move Tag To Screen Left",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Right", function()
			kbd_intent()
			actions.move_tag_to_screen("right")
		end, {
			description = "Move Tag To Screen Right",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Up", function()
			kbd_intent()
			actions.move_tag_to_screen("up")
		end, {
			description = "Move Tag To Screen Up",
			group = "Tags",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "Down", function()
			kbd_intent()
			actions.move_tag_to_screen("down")
		end, {
			description = "Move Tag To Screen Down",
			group = "Tags",
		}),

		awful.key({ modkey }, "n", function()
			kbd_intent()
			actions.add()
		end, {
			description = "Create New Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Shift" }, "n", function()
			kbd_intent()
			actions.add_silent()
		end, {
			description = "Create New Tag Silently",
			group = "Tags",
		}),

		awful.key({ modkey }, "c", function()
			kbd_intent()
			actions.delete_current()
		end, {
			description = "Close Current Tag",
			group = "Tags",
		}),

		awful.key({ modkey, "Shift" }, "c", function()
			kbd_intent()
			actions.delete_current_force()
		end, {
			description = "Force Close Current Tag",
			group = "Tags",
		})
	)
end

return function(modkey, actions)
	return M.build(modkey, actions)
end
