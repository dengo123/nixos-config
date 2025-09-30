-- ~/.config/awesome/input/keys/global/awesome.lua
local awful = require("awful")

return function(modkey)
	return awful.util.table.join(
		awful.key({ modkey }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
		awful.key({ modkey, "Control" }, "r", function()
			awful.spawn.with_shell(
				"systemctl --user daemon-reexec && systemctl --user restart graphical-session.target"
			)
			awesome.restart()
		end, { description = "reload session (services + awesome)", group = "awesome" }),
		awful.key({ modkey }, "q", function()
			local c = client.focus
			if c then
				c:kill()
			end
		end, { description = "close focused window", group = "client" })
	)
end
