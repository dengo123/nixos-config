-- features/shell/menu.lua
local hotkeys_popup = require("awful.hotkeys_popup")
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

function M.create(opts)
	opts = opts or {}
	local cfg = opts.cfg or {}
	local icon = opts.awesome_icon or beautiful.awesome_icon

	local myawesomemenu = {
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{ "manual", (cfg.terminal or "xterm") .. " -e man awesome" },
		{
			"edit config",
			(cfg.terminal or "xterm") .. " -e " .. (cfg.editor or "nano") .. " " .. awesome.conffile,
		},
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
			{ "awesome", myawesomemenu, icon },
			{ "open terminal", cfg.terminal or "xterm" },
		},
	})

	local mylauncher = awful.widget.launcher({ image = icon, menu = mymainmenu })
	return { menu = mymainmenu, launcher = mylauncher }
end

return M
