-- ~/.config/awesome/input/keys/global/layout.lua
local awful = require("awful")

local function promote_focused_to_master()
	local c = client.focus
	if not c then
		return
	end
	c:swap(awful.client.getmaster())
end

return function(modkey)
	return awful.util.table.join(
		-- Fenster zum Master (ehemals client.lua)
		awful.key({ modkey, "Shift" }, "Return", function()
			promote_focused_to_master()
		end, { description = "promote focused window to master", group = "layout" }),

		-- Optional beibehalten: Master-Breite justieren
		awful.key({ modkey }, "equal", function()
			awful.tag.incmwfact(0.05)
		end, { description = "master width +", group = "layout" }),
		awful.key({ modkey }, "minus", function()
			awful.tag.incmwfact(-0.05)
		end, { description = "master width -", group = "layout" })
	)
end
