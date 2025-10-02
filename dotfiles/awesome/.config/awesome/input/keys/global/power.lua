-- ~/.config/awesome/input/keys/global/power.lua
local awful = require("awful")

return function(modkey)
	local function bind(mods, key, fn, desc)
		return awful.key(mods, key, fn, { description = desc, group = "power" })
	end

	return awful.util.table.join(bind({ modkey }, "Escape", function()
		local Power = require("shell.menu.power")
		Power.open() -- öffnet mit deinem Fokus-Grabber aktiv
	end, "open power dialog"))
end
