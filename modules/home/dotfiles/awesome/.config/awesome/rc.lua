-- ~/.config/awesome/rc.lua
-- {{{ Loader
pcall(require, "luarocks.loader")

-- Standard libs
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")

-- ensure our config dir is on Lua's package.path (helps LSP & require)
local confdir = gears.filesystem.get_configuration_dir()
package.path = confdir .. "?.lua;" .. confdir .. "?/init.lua;" .. package.path

-- Own modules (passen, falls deine Dateien anders heißen/liegen)
local kb = require("keys")
kb.apply(cfg)
local tags = require("tags")
local mouse = require("mouse")
local bar = require("widgets.bar") -- z.B. ~/.config/awesome/widgets/bar.lua
local menu = require("widgets.menu") -- z.B. ~/.config/awesome/widgets/menu.lua
local rules = require("rules") -- ~/.config/awesome/rules.lua
local signals = require("signals") -- ~/.config/awesome/signals.lua
local cfg = require("config")
local theme = require("theme")
local layouts = require("layouts")
-- }}}

-- {{{ Error handling
if awesome.startup_errors then
	naughty.notify({
		preset = naughty.config.presets.critical,
		title = "Oops, there were errors during startup!",
		text = awesome.startup_errors,
	})
end

do
	local in_error = false
	awesome.connect_signal("debug::error", function(err)
		if in_error then
			return
		end
		in_error = true
		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Oops, an error happened!",
			text = tostring(err),
		})
		in_error = false
	end)
end
-- }}}

-- {{{ Menu / Launcher (aus Modul)
local mw = menu.create({
	terminal = terminal,
	editor_cmd = editor_cmd,
	awesome_icon = beautiful.awesome_icon,
})
local mylauncher = mw.launcher
local mymainmenu = mw.menu
menubar.utils.terminal = terminal
-- }}}

-- {{{ Keyboard & Mouse
local mykeyboardlayout = awful.widget.keyboardlayout()
mouse.apply_root(mymainmenu) -- Rechtsklick-Menu + Tag-Scroll am Root
-- }}}

-- {{{ Wallpaper helper
local function set_wallpaper(s)
	if beautiful.wallpaper then
		local wp = beautiful.wallpaper
		if type(wp) == "function" then
			wp = wp(s)
		end
		gears.wallpaper.maximized(wp, s, true)
	end
end
screen.connect_signal("property::geometry", set_wallpaper)
-- }}}

-- {{{ Screens: Tags + Bar pro Screen
awful.screen.connect_for_each_screen(function(s)
	set_wallpaper(s)
	tags.ensure(s)
	tags.renumber(s)

	bar.setup(s, {
		launcher = mylauncher,
		keyboardlayout = mykeyboardlayout,
	})
end)
-- }}}

-- {{{ Rules & Signals (ausgelagert)
rules.apply(kb, mouse)
signals.apply(mouse)
-- }}}

-- {{{
theme.apply(cfg)
layouts:apply()
-- }}}
