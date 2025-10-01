-- ~/.config/awesome/input/keys/global/layout.lua
local awful = require("awful")

-- NEU: Layout-Helfer laden (dein ~/.config/awesome/workspaces/layouts.lua)
local ok, layouts = pcall(require, "workspaces.layouts")
if not ok or type(layouts) ~= "table" then
	-- kleiner Fallback, damit die Keys trotzdem funktionieren
	layouts = {
		next = function()
			awful.layout.inc(1)
		end,
		prev = function()
			awful.layout.inc(-1)
		end,
		toggle_to_tile = function()
			local t = (awful.screen.focused() or {}).selected_tag
			if not t then
				return
			end
			local tile = awful.layout.suit.tile
			if t.layout == tile then
				awful.layout.inc(1)
			else
				t.layout = tile
			end
		end,
	}
end

local function promote_focused_to_master()
	local c = client.focus
	if not c then
		return
	end
	c:swap(awful.client.getmaster())
end

return function(modkey)
	return awful.util.table.join(
		-- promote to master
		awful.key(
			{ modkey, "Shift" },
			"Return",
			promote_focused_to_master,
			{ description = "promote focused window to master", group = "layout" }
		),

		-- master width
		awful.key({ modkey }, "equal", function()
			awful.tag.incmwfact(0.05)
		end, { description = "master width +", group = "layout" }),
		awful.key({ modkey }, "minus", function()
			awful.tag.incmwfact(-0.05)
		end, { description = "master width -", group = "layout" }),

		-- Layout wechseln (next/prev)
		awful.key({ modkey }, "Tab", layouts.next, { description = "next layout", group = "layout" }),
		awful.key({ modkey, "Shift" }, "Tab", layouts.prev, { description = "previous layout", group = "layout" }),

		-- Toggle-to-Tile
		awful.key({ modkey }, "space", function()
			layouts.toggle_to_tile()
		end, { description = "toggle to tile", group = "layout" })
	)
end
