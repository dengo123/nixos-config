local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function has_entries(t)
	return t and next(t) ~= nil
end

local function ucfirst(s)
	if not s or s == "" then
		return s
	end
	return s:sub(1, 1):upper() .. s:sub(2)
end

local function add_unique(t, v)
	if not v or v == "" then
		return
	end
	for _, existing in ipairs(t) do
		if existing == v then
			return
		end
	end
	table.insert(t, v)
end

local function build_file_manager_match(file_manager)
	if not file_manager or file_manager == "" then
		return nil
	end

	local bin = file_manager:match("^%s*([^%s]+)")
	if not bin or bin == "" then
		return nil
	end

	local base = bin:match("([^/]+)$") or bin
	if not base or base == "" then
		return nil
	end

	local classes = {}
	local instances = {}

	add_unique(instances, base)
	add_unique(classes, base)
	add_unique(classes, ucfirst(base))

	if base == "nautilus" then
		add_unique(classes, "org.gnome.Nautilus")
		add_unique(classes, "Org.gnome.Nautilus")
	elseif base == "pcmanfm-qt" then
		add_unique(classes, "Pcmanfm-qt")
	elseif base == "doublecmd" then
		add_unique(classes, "Double Commander")
	end

	return {
		class = classes,
		instance = instances,
	}
end

-- ============================================================================
-- Portrait Autosize
-- ============================================================================

local function portrait_autosize_apply(c)
	if not (c and c.valid) then
		return
	end
	if not c.portrait_autosize then
		return
	end
	if not c.floating or c.fullscreen or c.maximized then
		return
	end

	local s = c.screen
	if not s then
		return
	end

	local wa = s.workarea
	if wa.height <= wa.width then
		return
	end

	local w = math.floor(wa.width * 0.92)
	local h = math.floor(wa.height / 3)
	local x = wa.x + math.floor((wa.width - w) / 2)
	local y = wa.y + math.floor((wa.height - h) / 2)

	c:geometry({
		x = x,
		y = y,
		width = w,
		height = h,
	})
end

local function hook_portrait_autosize()
	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			portrait_autosize_apply(c)
		end)
	end)

	client.connect_signal("property::floating", function(c)
		portrait_autosize_apply(c)
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			portrait_autosize_apply(c)
		end
	end)
end

-- ============================================================================
-- Rules
-- ============================================================================

function M.apply(o)
	o = o or {}

	local modkey = o.modkey
	local mouse = o.mouse
	local cfg = o.cfg or {}

	local file_manager = cfg.system and cfg.system.files or nil
	local fm_match = build_file_manager_match(file_manager)

	local rules = {
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
			},
		},
	}

	-- ------------------------------------------------------------------------
	-- File Manager
	-- ------------------------------------------------------------------------

	if fm_match and (has_entries(fm_match.class) or has_entries(fm_match.instance)) then
		table.insert(rules, {
			rule_any = {
				class = has_entries(fm_match.class) and fm_match.class or nil,
				instance = has_entries(fm_match.instance) and fm_match.instance or nil,
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
				portrait_autosize = true,
			},
		})
	end

	-- ------------------------------------------------------------------------
	-- System Utilities
	-- ------------------------------------------------------------------------

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
			portrait_autosize = true,
		},
	})

	-- ------------------------------------------------------------------------
	-- XScreenSaver
	-- ------------------------------------------------------------------------

	table.insert(rules, {
		rule_any = {
			class = { ".xscreensaver-demo-wrapped", "XScreenSaver" },
			instance = { ".xscreensaver-demo-wrapped", "xscreensaver", "xscreensaver-demo" },
			name = { "XScreenSaver", "XScreenSaver Preferences" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
		},
	})

	-- ------------------------------------------------------------------------
	-- Generic Floaters
	-- ------------------------------------------------------------------------

	table.insert(rules, {
		rule_any = {
			type = { "dialog", "utility", "toolbar", "splash" },
			role = { "pop-up", "Preferences" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
		},
	})

	-- ------------------------------------------------------------------------
	-- Titlebars
	-- ------------------------------------------------------------------------

	table.insert(rules, {
		rule_any = { type = { "normal", "dialog" } },
		properties = { titlebars_enabled = true },
	})

	-- ------------------------------------------------------------------------
	-- Callbacks
	-- ------------------------------------------------------------------------

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
	hook_portrait_autosize()
end

return M
