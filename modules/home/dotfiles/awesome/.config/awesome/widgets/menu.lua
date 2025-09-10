-- ~/.config/awesome/widgets/menu.lua
local awful = require("awful")
local beautiful = require("beautiful")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")

local M = {}

--- Baut Hauptmenü + Launcher (1:1 aus deiner rc.lua, aber parametrisierbar).
-- @param opts { terminal, editor_cmd, awesome_icon }
-- @returns { menu = mymainmenu, launcher = mylauncher, awesomemenu = myawesomemenu }
function M.create(opts)
	opts = opts or {}
	local terminal = opts.terminal or "xterm"
	local editor_cmd = opts.editor_cmd or (terminal .. " -e nano")
	local awesome_icon = opts.awesome_icon or beautiful.awesome_icon

	local myawesomemenu = {
		{
			"hotkeys",
			function()
				hotkeys_popup.show_help(nil, awful.screen.focused())
			end,
		},
		{ "manual", terminal .. " -e man awesome" },
		{ "edit config", editor_cmd .. " " .. awesome.conffile },
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
			{ "awesome", myawesomemenu, awesome_icon },
			{ "open terminal", terminal },
		},
	})

	local mylauncher = awful.widget.launcher({
		image = awesome_icon,
		menu = mymainmenu,
	})

	-- menubar terminal setzen (wie in rc.lua)
	menubar.utils.terminal = terminal

	return {
		menu = mymainmenu,
		launcher = mylauncher,
		awesomemenu = myawesomemenu,
	}
end

return M
