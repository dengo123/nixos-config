local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

-- === File-Manager-Whitelist (WM_CLASS class/instance) =======================
local FM_CLASSES = {
	-- GNOME Files / Nautilus
	["Nautilus"] = true,
	["org.gnome.Nautilus"] = true,
	["Org.gnome.Nautilus"] = true,
	["nautilus"] = true,

	-- KDE Dolphin
	["Dolphin"] = true,
	["dolphin"] = true,

	-- Xfce Thunar
	["Thunar"] = true,
	["thunar"] = true,

	-- Cinnamon Nemo
	["Nemo"] = true,
	["nemo"] = true,

	-- LXDE/LXQt
	["Pcmanfm"] = true,
	["pcmanfm"] = true,
	["pcmanfm-qt"] = true,
	["Pcmanfm-qt"] = true,

	-- MATE
	["Caja"] = true,
	["caja"] = true,

	-- Andere
	["Spacefm"] = true,
	["spacefm"] = true,
	["Krusader"] = true,
	["krusader"] = true,
	["Double Commander"] = true,
	["doublecmd"] = true,
}

local function is_file_manager(c)
	if not c then
		return false
	end
	local cls = c.class or ""
	local inst = c.instance or ""
	return FM_CLASSES[cls] or FM_CLASSES[inst]
end

-- === Portrait-Sizing: volle Breite, 1/3 Höhe =================================
-- → hier NUR für File-Manager anwenden; setz 'only_for_fm=false',
--   wenn du es wieder global für alle Floating-Fenster willst.
local only_for_fm = true
local function size_floating_on_portrait(c)
	if not (c and c.valid) then
		return
	end
	if not c.floating or c.fullscreen or c.maximized then
		return
	end
	if only_for_fm and not is_file_manager(c) then
		return
	end

	local s = c.screen
	if not s then
		return
	end
	local wa = s.workarea
	local is_portrait = wa.height > wa.width
	if not is_portrait then
		return
	end

	local w = wa.width
	local h = math.floor(wa.height / 3)
	local x = wa.x
	local y = wa.y + math.floor((wa.height - h) / 2) -- oben: local y = wa.y

	c:geometry({ x = x, y = y, width = w, height = h })
end

local function hook_portrait_and_fm_floating()
	-- Fallback: File-Manager IMMER floating setzen (falls Rule nicht matcht)
	client.connect_signal("manage", function(c)
		if is_file_manager(c) and not c.floating then
			c.floating = true
		end
		gears.timer.delayed_call(size_floating_on_portrait, c)
	end)

	client.connect_signal("property::floating", function(c)
		size_floating_on_portrait(c)
	end)

	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			size_floating_on_portrait(c)
		end
	end)
end

function M.apply(o)
	local modkey = o.modkey
	local mouse = o.mouse

	awful.rules.rules = {
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
			},
		},

		-- File-Manager: immer floating + zentriert
		{
			rule_any = {
				class = {
					"Nautilus",
					"org.gnome.Nautilus",
					"Org.gnome.Nautilus",
					"Dolphin",
					"Thunar",
					"Nemo",
					"Pcmanfm",
					"Pcmanfm-qt",
					"Caja",
					"Spacefm",
					"Krusader",
					"Double Commander",
				},
				instance = {
					"nautilus",
					"dolphin",
					"thunar",
					"nemo",
					"pcmanfm",
					"pcmanfm-qt",
					"caja",
					"spacefm",
					"krusader",
					"doublecmd",
				},
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
			},
		},

		-- Deine Applets: floating + zentriert (aus vorigem Schritt)
		{
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
				name = { "Network Connections", "Bluetooth", "GNOME Disks", "Volume Control" },
			},
			properties = { floating = true, placement = awful.placement.centered },
		},

		-- Generische Utilities/Dialogs: floating
		{
			rule_any = {
				type = { "dialog", "utility", "toolbar", "splash" },
				role = { "pop-up", "Preferences" },
			},
			properties = { floating = true, placement = awful.placement.centered },
		},

		-- Titlebars
		{
			rule_any = { type = { "normal", "dialog" } },
			properties = { titlebars_enabled = true },
		},
	}

	-- Hooks aktivieren (Portrait-Sizing + FM-Fallback)
	hook_portrait_and_fm_floating()
end

return M
