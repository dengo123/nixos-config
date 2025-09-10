-- ~/.config/awesome/keys.lua
local gears = require("gears")
local awful = require("awful")
local tags = require("policy.tags")

local M = {}

-- helpers --------------------------------------------------------------

local function scr_in_dir(dir)
	local s = awful.screen.focused()
	return s and s:get_next_in_direction(dir) or nil
end

local function view_tag_idx(delta, s)
	s = s or awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	awful.tag.viewidx(delta, s)
end

local function move_tag_to_screen(dir)
	local s = awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local target = scr_in_dir(dir)
	if not target then
		return
	end
	local t = s.selected_tag
	t.screen = target
	t:view_only()
	awful.screen.focus(target)
end

local function move_client_dir(dir)
	local c = client.focus
	if not c then
		return
	end
	awful.client.swap.bydirection(dir, c, nil)
end

local function move_client_to_screen(dir)
	local c = client.focus
	if not c then
		return
	end
	local target = scr_in_dir(dir)
	if not target then
		return
	end
	c:move_to_screen(target)
	local t = target.selected_tag or target.tags and target.tags[1]
	if t then
		c:move_to_tag(t)
		t:view_only()
	end
	c:raise()
end

local function move_client_to_neighbor_tag(delta, follow)
	local s = awful.screen.focused()
	if not s or not s.selected_tag then
		return
	end
	local t = s.selected_tag
	local tags = s.tags
	local idx = t.index + delta
	local nt = tags[idx]
	local c = client.focus
	if c and nt then
		c:move_to_tag(nt)
		if follow then
			nt:view_only()
			client.focus = c
			c:raise()
		end
	end
end

-- ===============================================================
-- apply(cfg) baut globalkeys + clientkeys basierend auf cfg
-- ===============================================================
function M.apply(cfg)
	local modkey = cfg.modkey
	local term = cfg.terminal
	local launcher = cfg.launcher
	local browser = cfg.browser
	local files = cfg.files

	local globalkeys = gears.table.join(
		-- Fokus bewegen
		awful.key({ modkey }, "Left", function()
			awful.client.focus.bydirection("left")
		end, { description = "focus window left", group = "client" }),
		awful.key({ modkey }, "Right", function()
			awful.client.focus.bydirection("right")
		end, { description = "focus window right", group = "client" }),
		awful.key({ modkey }, "Up", function()
			awful.client.focus.bydirection("up")
		end, { description = "focus window up", group = "client" }),
		awful.key({ modkey }, "Down", function()
			awful.client.focus.bydirection("down")
		end, { description = "focus window down", group = "client" }),

		-- Fenster bewegen
		awful.key({ modkey, "Shift" }, "Left", function()
			move_client_dir("left")
		end, { description = "move window left", group = "client" }),
		awful.key({ modkey, "Shift" }, "Right", function()
			move_client_dir("right")
		end, { description = "move window right", group = "client" }),
		awful.key({ modkey, "Shift" }, "Up", function()
			move_client_dir("up")
		end, { description = "move window up", group = "client" }),
		awful.key({ modkey, "Shift" }, "Down", function()
			move_client_dir("down")
		end, { description = "move window down", group = "client" }),

		-- Fenster zwischen Tags verschieben (Fokus folgt)
		awful.key({ modkey, "Control", "Shift" }, "Right", function()
			move_client_to_neighbor_tag(1, true)
		end, { description = "move window to next tag (focus follows)", group = "client" }),
		awful.key({ modkey, "Control", "Shift" }, "Left", function()
			move_client_to_neighbor_tag(-1, true)
		end, { description = "move window to prev tag (focus follows)", group = "client" }),

		-- Fenster auf anderen Monitor
		awful.key({ modkey, "Shift", "Mod1" }, "Left", function()
			move_client_to_screen("left")
		end, { description = "move window to screen left", group = "client" }),
		awful.key({ modkey, "Shift", "Mod1" }, "Right", function()
			move_client_to_screen("right")
		end, { description = "move window to screen right", group = "client" }),
		awful.key({ modkey, "Shift", "Mod1" }, "Up", function()
			move_client_to_screen("up")
		end, { description = "move window to screen up", group = "client" }),
		awful.key({ modkey, "Shift", "Mod1" }, "Down", function()
			move_client_to_screen("down")
		end, { description = "move window to screen down", group = "client" }),

		-- Tag wechseln
		awful.key({ modkey, "Control" }, "Right", function()
			view_tag_idx(1)
		end, { description = "next tag (on screen)", group = "tag" }),
		awful.key({ modkey, "Control" }, "Left", function()
			view_tag_idx(-1)
		end, { description = "prev tag (on screen)", group = "tag" }),

		-- Tag verschieben
		awful.key({ modkey, "Control", "Mod1" }, "Left", function()
			move_tag_to_screen("left")
		end, { description = "move tag to screen left", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Right", function()
			move_tag_to_screen("right")
		end, { description = "move tag to screen right", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Up", function()
			move_tag_to_screen("up")
		end, { description = "move tag to screen up", group = "tag" }),
		awful.key({ modkey, "Control", "Mod1" }, "Down", function()
			move_tag_to_screen("down")
		end, { description = "move tag to screen down", group = "tag" }),

		-- Monitor wechseln
		awful.key({ modkey, "Mod1" }, "Left", function()
			local t = scr_in_dir("left")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen left", group = "screen" }),
		awful.key({ modkey, "Mod1" }, "Right", function()
			local t = scr_in_dir("right")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen right", group = "screen" }),
		awful.key({ modkey, "Mod1" }, "Up", function()
			local t = scr_in_dir("up")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen up", group = "screen" }),
		awful.key({ modkey, "Mod1" }, "Down", function()
			local t = scr_in_dir("down")
			if t then
				awful.screen.focus(t)
			end
		end, { description = "focus screen down", group = "screen" }),

		-- Dynamische Tags
		awful.key({ modkey }, "n", function()
			tags.add()
		end, { description = "new tag (auto)", group = "tag" }),
		awful.key({ modkey }, "c", function()
			tags.delete_current()
		end, { description = "close current tag", group = "tag" }),

		-- Layout
		awful.key({ modkey }, "Tab", function()
			awful.layout.inc(1)
		end, { description = "next layout", group = "layout" }),
		awful.key({ modkey, "Shift" }, "Tab", function()
			awful.layout.inc(-1)
		end, { description = "prev layout", group = "layout" }),
		awful.key({ modkey }, "equal", function()
			awful.tag.incmwfact(0.05)
		end, { description = "master width +", group = "layout" }),
		awful.key({ modkey }, "minus", function()
			awful.tag.incmwfact(-0.05)
		end, { description = "master width -", group = "layout" }),

		-- States
		awful.key({ modkey }, "f", function()
			local c = client.focus
			if c then
				c.fullscreen = not c.fullscreen
				c:raise()
			end
		end, { description = "toggle fullscreen", group = "client" }),
		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c then
				c.maximized = not c.maximized
				c:raise()
			end
		end, { description = "toggle maximize", group = "client" }),
		awful.key({ modkey }, "t", function()
			local c = client.focus
			if c then
				awful.client.floating.toggle(c)
			end
		end, { description = "toggle floating", group = "client" }),

		-- Apps
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
		end, { description = "screenshot", group = "launcher" }),

		-- Suche / Prompt (Mod + s)
		awful.key({ modkey }, "s", function()
			local s = awful.screen.focused()
			if s and s.mypromptbox then
				s.mypromptbox:run()
			end
		end, { description = "search / run prompt", group = "launcher" }),

		-- Awesome
		awful.key({ modkey }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
		awful.key({ modkey }, "q", function()
			local c = client.focus
			if c then
				c:kill()
			end
		end, { description = "close focused window", group = "client" })
	)

	-- Client Keys
	local clientkeys = gears.table.join(awful.key({ modkey, "Shift" }, "Return", function(c)
		c:swap(awful.client.getmaster())
	end, { description = "promote to master", group = "client" }))

	-- speichern/exportieren
	M.globalkeys = globalkeys
	M.clientkeys = clientkeys
	root.keys(globalkeys)
end

return M
