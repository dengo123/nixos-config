-- ~/.config/awesome/features/shell/menu/dialogs/hotkeys.lua
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
pcall(require, "awful.hotkeys_popup.keys") -- Gruppen registrieren

local M = {}

-- >>> Hier Theme direkt im Modul festlegen
local LOCAL_THEME = {
	bg = "#ECE9D8", -- Hintergrund
	fg = "#000000", -- Textfarbe
	border = "#2b77ff", -- Randfarbe
	border_width = 2, -- Randbreite (px)
	radius = 10, -- Eckenradius (px)
	font = nil, -- z.B. "Inter 10"
	desc_font = nil, -- z.B. "Inter 10"
	modifiers_fg = "#000000", -- Farbe der Modifikatorkeys
	group_margin = 6, -- Abstand zwischen Gruppen
}

-- Optional: per Code änderbar (falls du später willst)
function M.set_theme(t)
	for k, v in pairs(t or {}) do
		LOCAL_THEME[k] = v
	end
end

local function apply_theme()
	beautiful.hotkeys_bg = LOCAL_THEME.bg
	beautiful.hotkeys_fg = LOCAL_THEME.fg
	beautiful.hotkeys_border_color = LOCAL_THEME.border
	beautiful.hotkeys_border_width = LOCAL_THEME.border_width
	beautiful.hotkeys_shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, LOCAL_THEME.radius or 0)
	end

	if LOCAL_THEME.font then
		beautiful.hotkeys_font = LOCAL_THEME.font
	end
	if LOCAL_THEME.desc_font then
		beautiful.hotkeys_description_font = LOCAL_THEME.desc_font
	end
	if LOCAL_THEME.modifiers_fg ~= nil then
		beautiful.hotkeys_modifiers_fg = LOCAL_THEME.modifiers_fg
	end
	if LOCAL_THEME.group_margin ~= nil then
		beautiful.hotkeys_group_margin = LOCAL_THEME.group_margin
	end
end

-- Minimal: öffnet immer das klassische Hotkeys-Popup mit unserem Modul-Theme.
-- Gibt ein 1x1-Placeholder-Widget zurück (damit dein Overlay-Fluss zufrieden ist).
function M.hotkeys(_opts)
	apply_theme()
	hotkeys_popup.show_help(nil, awful.screen.focused())

	return wibox.widget({
		strategy = "exact",
		width = 1,
		height = 1,
		widget = wibox.container.constraint,
	})
end

return M
