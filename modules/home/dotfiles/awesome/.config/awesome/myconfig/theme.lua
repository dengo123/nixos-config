-- ~/.config/awesome/theme.lua
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local theme = {}

-- XP‑angehauchte Farben/Fonts
theme.font = "Jetbrains Mono"
theme.bg_normal = "#d6d6ce" -- Taskbar hellgrau
theme.bg_focus = "#245EDC" -- Luna-Blau
theme.bg_urgent = "#e81123"
theme.bg_minimize = "#cfcfc8"

theme.fg_normal = "#000000"
theme.fg_focus = "#ffffff"
theme.fg_urgent = "#ffffff"

theme.border_width = dpi(1)
theme.border_normal = "#999999"
theme.border_focus = "#245EDC"

-- Wibar
theme.wibar_height = dpi(28)

-- Spacing
theme.useless_gap = dpi(4)

return theme
