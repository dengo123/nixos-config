-- ~/.config/awesome/input/keys/global/prompt.lua
local awful = require("awful")

return function(modkey)
	return awful.key({ modkey }, "s", function()
		local s = awful.screen.focused()
		if s and s.mypromptbox then
			s.mypromptbox:run()
		end
	end, { description = "search / run prompt", group = "launcher" })
end
