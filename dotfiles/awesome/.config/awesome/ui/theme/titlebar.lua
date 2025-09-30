-- ~/.config/awesome/ui/theme/titlebar.lua
local beautiful = require("beautiful")
local fs = require("gears.filesystem")

local T = {}

function T.init(_cfg)
	-- Farben (neutral-grau)
	beautiful.titlebar_bg_normal = beautiful.titlebar_bg_normal or (beautiful.bg_normal or "#222222")
	beautiful.titlebar_bg_focus = beautiful.titlebar_bg_focus or (beautiful.bg_focus or "#535d6c")
	beautiful.titlebar_fg_normal = beautiful.titlebar_fg_normal or (beautiful.fg_normal or "#DDDDDD")
	beautiful.titlebar_fg_focus = beautiful.titlebar_fg_focus or (beautiful.fg_focus or "#FFFFFF")

	-- Button-Assets aus dem Default-Theme referenzieren
	local base = fs.get_themes_dir() .. "default/titlebar/"

	-- Close
	beautiful.titlebar_close_button_normal = beautiful.titlebar_close_button_normal or (base .. "close_normal.png")
	beautiful.titlebar_close_button_focus = beautiful.titlebar_close_button_focus or (base .. "close_focus.png")

	-- Minimize
	beautiful.titlebar_minimize_button_normal = beautiful.titlebar_minimize_button_normal
		or (base .. "minimize_normal.png")
	beautiful.titlebar_minimize_button_focus = beautiful.titlebar_minimize_button_focus
		or (base .. "minimize_focus.png")

	-- Floating (active/inactive, normal/focus)
	beautiful.titlebar_floating_button_normal_active = beautiful.titlebar_floating_button_normal_active
		or (base .. "floating_normal_active.png")
	beautiful.titlebar_floating_button_focus_active = beautiful.titlebar_floating_button_focus_active
		or (base .. "floating_focus_active.png")
	beautiful.titlebar_floating_button_normal_inactive = beautiful.titlebar_floating_button_normal_inactive
		or (base .. "floating_normal_inactive.png")
	beautiful.titlebar_floating_button_focus_inactive = beautiful.titlebar_floating_button_focus_inactive
		or (base .. "floating_focus_inactive.png")

	-- (Optional) weitere Buttons, falls du sie später nutzt:
	-- maximize, ontop, sticky …
	-- beautiful.titlebar_maximized_button_normal_active = base .. "maximized_normal_active.png"
	-- beautiful.titlebar_maximized_button_focus_active  = base .. "maximized_focus_active.png"
	-- …
end

return T
