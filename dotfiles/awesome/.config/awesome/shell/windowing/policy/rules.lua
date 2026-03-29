-- ~/.config/awesome/shell/windowing/policy/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply(o)
	o = o or {}

	local cfg = o.cfg or {}

	local Clients = o.clients
	local Floating = o.floating
	local Titlebars = o.titlebars

	local apps_cfg = cfg.apps or {}
	local windowing_cfg = cfg.windowing or {}
	local floating_cfg = windowing_cfg.floating or {}

	local file_manager = apps_cfg.files
	local terminal = apps_cfg.terminal

	local files_floating = (floating_cfg.files ~= false)
	local terminals_floating = (floating_cfg.terminals == true)

	local fm_match = Clients and Clients.build_file_manager_match and Clients.build_file_manager_match(file_manager)
		or nil
	local term_match = Clients and Clients.build_terminal_match and Clients.build_terminal_match(terminal) or nil

	local rules = {
		{
			rule = {},
			properties = {
				border_width = beautiful.border_width,
				border_color = beautiful.border_normal,
				focus = awful.client.focus.filter,
				raise = true,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen,
			},
		},
	}

	if files_floating and fm_match and Clients and Clients.has_entries then
		if Clients.has_entries(fm_match.class) or Clients.has_entries(fm_match.instance) then
			table.insert(rules, {
				rule_any = {
					class = Clients.has_entries(fm_match.class) and fm_match.class or nil,
					instance = Clients.has_entries(fm_match.instance) and fm_match.instance or nil,
				},
				properties = {
					floating = true,
					placement = awful.placement.centered,
					portrait_autosize = true,
					centered_autosize = false,
				},
			})
		end
	end

	if terminals_floating and term_match and Clients and Clients.has_entries then
		if Clients.has_entries(term_match.class) or Clients.has_entries(term_match.instance) then
			table.insert(rules, {
				rule_any = {
					class = Clients.has_entries(term_match.class) and term_match.class or nil,
					instance = Clients.has_entries(term_match.instance) and term_match.instance or nil,
				},
				properties = {
					floating = true,
					placement = awful.placement.centered,
					portrait_autosize = false,
					centered_autosize = true,
				},
				callback = function(c)
					if Floating and type(Floating.reinforce_tall_centered_client) == "function" then
						Floating.reinforce_tall_centered_client(c)
					end
				end,
			})
		end
	end

	table.insert(rules, {
		rule_any = {
			class = { "copyq", "CopyQ" },
			instance = { "copyq" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = false,
			centered_autosize = true,
		},
		callback = function(c)
			if Floating and type(Floating.reinforce_tall_centered_client) == "function" then
				Floating.reinforce_tall_centered_client(c)
			end
		end,
	})

	table.insert(rules, {
		rule_any = {
			class = {
				"gnome-calendar",
				"org.gnome.Calendar",
				"Org.gnome.Calendar",
				"Gnome-calendar",
				"Gnome-Calendar",
			},
			instance = { "gnome-calendar" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
			centered_autosize = false,
		},
		callback = function(c)
			if Floating and type(Floating.portrait_autosize_apply) == "function" then
				Floating.portrait_autosize_apply(c)
			end
		end,
	})

	table.insert(rules, {
		rule_any = {
			class = {
				"Nm-connection-editor",
				"Blueman-manager",
				"Pavucontrol",
				"Gnome-disks",
				"Org.gnome.DiskUtility",
				"org.gnome.DiskUtility",
				".xscreensaver-demo-wrapped",
				"XScreenSaver",
			},
			instance = {
				"nm-connection-editor",
				"blueman-manager",
				"pavucontrol",
				"gnome-disks",
				".xscreensaver-demo-wrapped",
				"xscreensaver",
				"xscreensaver-demo",
			},
			name = {
				"Network Connections",
				"Bluetooth",
				"GNOME Disks",
				"Volume Control",
				"XScreenSaver",
				"XScreenSaver Preferences",
			},
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
			centered_autosize = false,
		},
	})

	table.insert(rules, {
		rule_any = {
			type = { "dialog", "utility", "toolbar", "splash" },
			role = { "pop-up", "Preferences" },
		},
		properties = {
			floating = true,
			placement = awful.placement.centered,
			portrait_autosize = true,
			centered_autosize = false,
		},
	})

	table.insert(rules, {
		rule_any = {
			type = { "normal", "dialog" },
		},
		properties = {},
		callback = function(c)
			if Titlebars and type(Titlebars.enabled_for) == "function" then
				c.titlebars_enabled = Titlebars.enabled_for(c)
			end
		end,
	})

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

	if Floating and type(Floating.init) == "function" then
		Floating.init({
			clients = Clients,
		})
	end
end

return M
