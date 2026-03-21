-- ~/.config/awesome/input/global/layout.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function promote_focused_to_master()
	local c = client.focus
	if not c then
		return
	end

	c:swap(awful.client.getmaster())
end

local function next_layout()
	awful.layout.inc(1)
end

local function previous_layout()
	awful.layout.inc(-1)
end

local function increase_master_width()
	awful.tag.incmwfact(0.05)
end

local function decrease_master_width()
	awful.tag.incmwfact(-0.05)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey)
	return awful.util.table.join(
		awful.key({ modkey, "Control" }, "Tab", function()
			promote_focused_to_master()
		end, {
			description = "Promote Focused Window To Master",
			group = "Layout",
		}),

		awful.key({ modkey }, "equal", function()
			increase_master_width()
		end, {
			description = "Increase Master Width",
			group = "Layout",
		}),

		awful.key({ modkey }, "minus", function()
			decrease_master_width()
		end, {
			description = "Decrease Master Width",
			group = "Layout",
		}),

		awful.key({ modkey }, "Tab", function()
			next_layout()
		end, {
			description = "Next Layout",
			group = "Layout",
		}),

		awful.key({ modkey, "Shift" }, "Tab", function()
			previous_layout()
		end, {
			description = "Previous Layout",
			group = "Layout",
		})
	)
end

return function(modkey)
	return M.build(modkey)
end
