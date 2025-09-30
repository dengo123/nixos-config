-- ~/.config/awesome/input/keys/global/apps.lua
local awful = require("awful")

return function(modkey, cfg)
	local term, launcher, browser, files = cfg.terminal, cfg.launcher, cfg.browser, cfg.files
	return awful.util.table.join(
		awful.key({ modkey }, "Return", function()
			awful.spawn(term)
		end, { description = "open terminal", group = "launcher" }),
		awful.key({ modkey }, "space", function()
			awful.spawn(launcher)
		end, { description = "launcher/menu", group = "launcher" }),
		awful.key({ modkey }, "b", function()
			awful.spawn.with_shell(browser)
		end, { description = "browser", group = "launcher" }),
		awful.key({ modkey }, "e", function()
			awful.spawn.with_shell(files)
		end, { description = "file manager", group = "launcher" }),
		awful.key({ modkey }, "Print", function()
			awful.spawn.with_shell("screenshot")
		end, { description = "screenshot", group = "launcher" })
	)
end
