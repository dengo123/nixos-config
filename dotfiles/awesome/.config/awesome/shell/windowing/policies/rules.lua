-- ~/.config/awesome/shell/windowing/policies/rules.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

-- === File-Manager-Whitelist (WM_CLASS class/instance) =======================
local FM_CLASSES = {
	["Nautilus"] = true,
	["org.gnome.Nautilus"] = true,
	["Org.gnome.Nautilus"] = true,
	["nautilus"] = true,
	["Dolphin"] = true,
	["dolphin"] = true,
	["Thunar"] = true,
	["thunar"] = true,
	["Nemo"] = true,
	["nemo"] = true,
	["Pcmanfm"] = true,
	["pcmanfm"] = true,
	["pcmanfm-qt"] = true,
	["Pcmanfm-qt"] = true,
	["Caja"] = true,
	["caja"] = true,
	["Spacefm"] = true,
	["spacefm"] = true,
	["Krusader"] = true,
	["krusader"] = true,
	["Double Commander"] = true,
	["doublecmd"] = true,
}
local function is_file_manager(c)
	if not (c and c.valid) then
		return false
	end
	local cls, inst = c.class or "", c.instance or ""
	return FM_CLASSES[cls] or FM_CLASSES[inst]
end

-- === Terminal-Whitelist =====================================================
local TERM_CLASSES = {
	["Alacritty"] = true,
	["alacritty"] = true,
	["kitty"] = true,
	["Kitty"] = true,
	["st"] = true,
	["st-256color"] = true,
	["URxvt"] = true,
	["urxvt"] = true,
	["XTerm"] = true,
	["xterm"] = true,
	["Gnome-terminal"] = true,
	["gnome-terminal"] = true,
	["org.gnome.Terminal"] = true,
	["Org.gnome.Terminal"] = true,
	["Konsole"] = true,
	["konsole"] = true,
	["xfce4-terminal"] = true,
	["termite"] = true,
	["Tilix"] = true,
	["tilix"] = true,
	["WezTerm"] = true,
	["wezterm"] = true,
	["foot"] = true,
	["footclient"] = true,
}
local function is_terminal(c)
	if not (c and c.valid) then
		return false
	end
	local cls, inst = c.class or "", c.instance or ""
	return TERM_CLASSES[cls] or TERM_CLASSES[inst]
end

-- === Portrait-Autosize Kernlogik ============================================
local function portrait_autosize_apply(c)
	if not (c and c.valid) then
		return
	end
	-- Nur wenn die Regel/Property gesetzt wurde:
	if not c.portrait_autosize then
		return
	end

	-- Wie bisher: nur floating, nicht maximized/fullscreen
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
	end -- nur im Portrait-Screen

	local w = wa.width
	local h = math.floor(wa.height / 3)
	local x = wa.x
	local y = wa.y + math.floor((wa.height - h) / 2)
	c:geometry({ x = x, y = y, width = w, height = h })
end

local function hook_portrait_autosize()
	-- Beim Manage: nach dem Setzen der Rules kurz anwenden
	client.connect_signal("manage", function(c)
		-- File-Manager standardmäßig floating, siehe Rules unten
		gears.timer.delayed_call(portrait_autosize_apply, c)
	end)

	-- Wenn der Floating-State wechselt, ggf. neu anwenden
	client.connect_signal("property::floating", function(c)
		portrait_autosize_apply(c)
	end)

	-- Bei Screen-Geometrieänderung alle betroffenen Clients des Screens prüfen
	screen.connect_signal("property::geometry", function(s)
		for _, c in ipairs(s.clients) do
			portrait_autosize_apply(c)
		end
	end)
end

-- =============================================================================
function M.apply(o)
	o = o or {}
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

		-- File-Manager: floating + centered + Portrait-Autosize aktiv
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
				portrait_autosize = true, -- <── NEU: nur diese kriegen die Portrait-Auto-Größe
			},
		},

		-- Deine Applets/Prefs: floating + centered (ohne Portrait-Autosize)
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
				instance = { "nm-connection-editor", "blueman-manager", "pavucontrol", "gnome-disks" },
				name = { "Network Connections", "Bluetooth", "GNOME Disks", "Volume Control" },
			},
			properties = {
				floating = true,
				placement = awful.placement.centered,
				portrait_autosize = true,
			},
		},

		-- XScreenSaver Demo/Prefs
		{
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
		},

		-- Generische Utilities/Dialogs: floating
		{
			rule_any = { type = { "dialog", "utility", "toolbar", "splash" }, role = { "pop-up", "Preferences" } },
			properties = {
				floating = true,
				placement = awful.placement.centered,
				portrait_autosize = true,
			},
		},

		-- Titlebars standardmäßig an
		{ rule_any = { type = { "normal", "dialog" } }, properties = { titlebars_enabled = true } },

		-- Terminals: tiled, Titlebar an (KEIN portrait_autosize → werden nicht angerührt)
		{
			rule_any = {
				class = {
					"Alacritty",
					"alacritty",
					"kitty",
					"Kitty",
					"st",
					"st-256color",
					"URxvt",
					"urxvt",
					"XTerm",
					"xterm",
					"Gnome-terminal",
					"gnome-terminal",
					"org.gnome.Terminal",
					"Org.gnome.Terminal",
					"Konsole",
					"konsole",
					"xfce4-terminal",
					"termite",
					"Tilix",
					"tilix",
					"WezTerm",
					"wezterm",
					"foot",
					"footclient",
				},
				instance = {
					"alacritty",
					"kitty",
					"st",
					"urxvt",
					"xterm",
					"gnome-terminal",
					"konsole",
					"xfce4-terminal",
					"termite",
					"tilix",
					"wezterm",
					"foot",
					"footclient",
				},
			},
			properties = {
				floating = false,
				titlebars_enabled = false,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen,
				-- kein portrait_autosize hier ⇒ Terminals bleiben unbeeinflusst
			},
		},

		-- Fallback: Alles mit Titelbeginn "XScreenSaver:" zentrieren + floating
		{
			rule = {},
			properties = {},
			callback = function(c)
				if c.name and c.name:match("^XScreenSaver:") then
					c.floating = true
					awful.placement.centered(c)
				end
			end,
		},
	}

	-- Hooks aktivieren (arbeitet NUR auf Clients mit properties.portrait_autosize=true)
	hook_portrait_autosize()
end

return M
