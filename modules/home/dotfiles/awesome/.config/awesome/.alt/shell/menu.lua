local hotkeys_popup = require("awful.hotkeys_popup")
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}
function M.create(opts)
	local cfg = opts.cfg
	local myawesomemenu = {
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{ "manual", cfg.terminal .. " -e man awesome" },
		{ "edit config", cfg.terminal .. " -e " .. cfg.editor .. " " .. awesome.conffile },
		{ "restart", awesome.restart },
		{
			"quit",
			function()
				awesome.quit()
			end,
		},
	}
	local mymainmenu = awful.menu({
		items = {
			{ "awesome", myawesomemenu, opts.awesome_icon or beautiful.awesome_icon },
			{ "open terminal", cfg.terminal },
		},
	})
	local mylauncher = awful.widget.launcher({
		image = opts.awesome_icon or beautiful.awesome_icon,
		menu = mymainmenu,
	})
	return { menu = mymainmenu, launcher = mylauncher }
end

return M
