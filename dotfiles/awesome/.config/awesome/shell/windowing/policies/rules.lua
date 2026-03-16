-- ~/.config/awesome/shell/windowing/policies/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function first_token(x)
	if type(x) == "table" then
		return x[1]
	elseif type(x) == "string" then
		return x:match("^(%S+)")
	end
	return nil
end

local function basename(x)
	local t = first_token(x)
	if not t then
		return nil
	end
	return t:match("([^/]+)$")
end

local function capitalize_ascii(s)
	if not s or s == "" then
		return s
	end
	return s:gsub("^%l", string.upper)
end

local function build_app_rule_any(cmd)
	local app = basename(cmd)
	if not app or app == "xdg-open" then
		return nil
	end

	local lower = tostring(app):lower()
	local upper = capitalize_ascii(lower)

	return {
		class = { lower, upper },
		instance = { lower, upper },
	}
end

local function is_portrait_screen(s)
	if not (s and s.valid and s.workarea) then
		return false
	end
	local wa = s.workarea
	return wa.height > wa.width
end

local function portrait_apply(c, enabled)
	if not enabled then
		return
	end
	if not (c and c.valid) then
		return
	end
	if c.floating then
		return
	end

	local s = c.screen
	if not is_portrait_screen(s) then
		return
	end

	if not c.fullscreen then
		c.fullscreen = true
	end
end

local function portrait_reset(c)
	if not (c and c.valid) then
		return
	end
	local s = c.screen
	if is_portrait_screen(s) then
		return
	end
	if c.fullscreen then
		c.fullscreen = false
	end
end

local function hook_portrait_policy(enabled)
	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			portrait_apply(c, enabled)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		if c.floating then
			return
		end
		portrait_apply(c, enabled)
	end)

	client.connect_signal("property::screen", function(c)
		if is_portrait_screen(c.screen) then
			portrait_apply(c, enabled)
		else
			portrait_reset(c)
		end
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			if is_portrait_screen(s) then
				portrait_apply(c, enabled)
			else
				portrait_reset(c)
			end
		end
	end)
end

-- ============================================================================
-- Rules
-- ============================================================================

function M.apply(o)
	o = o or {}

	local cfg = o.cfg or {}
	local system_cfg = cfg.system or {}
	local windowing_cfg = cfg.windowing or {}
	local titlebars_cfg = windowing_cfg.titlebars or {}
	local portrait_cfg = windowing_cfg.portrait or {}

	local modkey = o.modkey
	local mouse = o.mouse

	local titlebars_enabled = (titlebars_cfg.enabled ~= false)
	local portrait_fullscreen_tiled = (portrait_cfg.fullscreen_tiled == true)

	local fm_rule_any = build_app_rule_any(system_cfg.files)

	local rules = {
		-- Default
		{
			rule = {},
			properties = {
				border_width = beautiful.border_width,
				border_color = beautiful.border_normal,
				focus = awful.client.focus.filter,
				raise = true,
				buttons = mouse and mouse.client_buttons and mouse.client_buttons(modkey) or nil,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen,
				titlebars_enabled = titlebars_enabled,
			},
		},
	}

	-- File manager from system.files: floating + centered
	if fm_rule_any then
		table.insert(rules, {
			rule_any = fm_rule_any,
			properties = {
				floating = true,
				placement = awful.placement.centered,
				titlebars_enabled = titlebars_enabled,
			},
		})
	end

	-- Dialoge / Utilities / Splash
	table.insert(rules, {
		rule_any = {
			type = { "dialog", "utility", "toolbar", "splash" },
			role = { "pop-up", "Preferences" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			titlebars_enabled = titlebars_enabled,
		},
	})

	-- Häufige Tools / Applets
	table.insert(rules, {
		rule_any = {
			class = {
				"Nm-connection-editor",
				"Blueman-manager",
				"Pavucontrol",
				"Gnome-disks",
				"Org.gnome.DiskUtility",
				"org.gnome.DiskUtility",
			},
			instance = {
				"nm-connection-editor",
				"blueman-manager",
				"pavucontrol",
				"gnome-disks",
			},
			name = {
				"Network Connections",
				"Bluetooth",
				"GNOME Disks",
				"Volume Control",
			},
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			titlebars_enabled = titlebars_enabled,
		},
	})

	-- XScreenSaver
	table.insert(rules, {
		rule_any = {
			class = { ".xscreensaver-demo-wrapped", "XScreenSaver" },
			instance = { ".xscreensaver-demo-wrapped", "xscreensaver", "xscreensaver-demo" },
			name = { "XScreenSaver", "XScreenSaver Preferences" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			titlebars_enabled = titlebars_enabled,
		},
	})

	-- Fallback: XScreenSaver anhand Fenstertitel
	table.insert(rules, {
		rule = {},
		properties = {},
		callback = function(c)
			if c.name and c.name:match("^XScreenSaver:") then
				c.floating = true
				awful.placement.centered(c)
			end
		end,
	})

	awful.rules.rules = rules

	hook_portrait_policy(portrait_fullscreen_tiled)
end

return M
