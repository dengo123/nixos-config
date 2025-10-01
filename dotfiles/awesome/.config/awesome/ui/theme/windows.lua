-- ~/.config/awesome/ui/theme/windows.lua
local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local W = {}

-- Kopiert nur fehlende Titlebar-Button-Assets aus dem Default-Theme,
-- ohne dein globales Theme zu überschreiben.
local function ensure_titlebar_assets()
	-- Wenn irgendein Asset bereits vorhanden ist, gehen wir davon aus, dass sie gesetzt sind.
	if
		beautiful.titlebar_close_button_normal
		or beautiful.titlebar_minimize_button_normal
		or beautiful.titlebar_maximized_button_normal
		or beautiful.titlebar_floating_button_normal
	then
		return
	end

	local default_theme_path = gears.filesystem.get_themes_dir() .. "default/theme.lua"
	local ok, def = pcall(dofile, default_theme_path)
	if not ok or type(def) ~= "table" then
		return
	end

	local keys = {
		-- Close
		"titlebar_close_button_normal",
		"titlebar_close_button_focus",
		-- Minimize
		"titlebar_minimize_button_normal",
		"titlebar_minimize_button_focus",
		-- Maximize (falls du es irgendwann nutzen willst)
		"titlebar_maximized_button_normal_active",
		"titlebar_maximized_button_focus_active",
		"titlebar_maximized_button_normal_inactive",
		"titlebar_maximized_button_focus_inactive",
		-- Floating
		"titlebar_floating_button_normal_active",
		"titlebar_floating_button_focus_active",
		"titlebar_floating_button_normal_inactive",
		"titlebar_floating_button_focus_inactive",
		-- Optional: Ontop/Sticky
		"titlebar_ontop_button_normal_active",
		"titlebar_ontop_button_focus_active",
		"titlebar_ontop_button_normal_inactive",
		"titlebar_ontop_button_focus_inactive",
		"titlebar_sticky_button_normal_active",
		"titlebar_sticky_button_focus_active",
		"titlebar_sticky_button_normal_inactive",
		"titlebar_sticky_button_focus_inactive",
	}

	for _, k in ipairs(keys) do
		if beautiful[k] == nil and def[k] ~= nil then
			beautiful[k] = def[k]
		end
	end
end

--[[
cfg = {
  colors = {
    normal_bg = "#222222",
    normal_fg = "#DDDDDD",
    focus_bg  = "#4C6EF5",
    focus_fg  = "#FFFFFF",
  },
  border_width   = 2,
  border_radius  = 8,
  border_normal  = "#444444",
  border_focus   = "#7aa2f7",
  titlebar = {
    position          = "top",
    height            = 28,
    use_border_colors = true, -- Titlebar-Farben = Rahmenfarben (Default)
  },
}
--]]

function W.init(cfg)
	cfg = cfg or {}

	-- Nur fehlende Titlebar-Assets selektiv befüllen, NICHT beautiful.init() aufrufen!
	ensure_titlebar_assets()

	-- Overrides auf das, was wir wirklich brauchen (restliches Theme bleibt unangetastet)
	local C = cfg.colors or {}
	local tb = cfg.titlebar or {}

	beautiful.border_width = (cfg.border_width ~= nil) and cfg.border_width or (beautiful.border_width or 2)
	beautiful.border_radius = (cfg.border_radius ~= nil) and cfg.border_radius or (beautiful.border_radius or 8)

	beautiful.border_normal = cfg.border_normal or beautiful.border_normal or "#0B89E7"
	beautiful.border_focus = cfg.border_focus or beautiful.border_focus or "#235CDB"

	beautiful.titlebar_position = tb.position or beautiful.titlebar_position or "top"
	beautiful.titlebar_height = tb.height or beautiful.titlebar_height or 28

	local use_border_colors = (tb.use_border_colors ~= false)
	if use_border_colors then
		beautiful.titlebar_bg_normal = beautiful.border_normal
		beautiful.titlebar_bg_focus = beautiful.border_focus
	else
		beautiful.titlebar_bg_normal = C.normal_bg or beautiful.titlebar_bg_normal or beautiful.bg_normal or "#0B89E7"
		beautiful.titlebar_bg_focus = C.focus_bg or beautiful.titlebar_bg_focus or beautiful.bg_focus or "#235CDB"
	end

	beautiful.titlebar_fg_normal = C.normal_fg or beautiful.titlebar_fg_normal or beautiful.fg_normal or "#DDDDDD"
	beautiful.titlebar_fg_focus = C.focus_fg or beautiful.titlebar_fg_focus or beautiful.fg_focus or "#FFFFFF"
end

-- Titlebar: Titel LINKS (Icon + Titel), MIDDLE = Drag-Spacer, RIGHT = Buttons
-- Mittlerer Button = TOGGLE FLOATING
function W.attach_titlebar(c, mouse, opts)
	if not (c and c.valid) then
		return
	end

	local pos = (opts and opts.position) or beautiful.titlebar_position or "top"
	local size = (opts and opts.size) or beautiful.titlebar_height or 28
	local buttons = (mouse and mouse.titlebar_buttons and mouse.titlebar_buttons(c)) or nil

	local title = awful.titlebar.widget.titlewidget(c)
	if title.set_align then
		title:set_align("left")
	end

	awful
		.titlebar(c, {
			position = pos,
			size = size,
			-- keine bg/fg hier: Farben kommen aus beautiful.*
		})
		:setup({
			{ -- LEFT: Icon + Titel (drag)
				awful.titlebar.widget.iconwidget(c),
				title,
				buttons = buttons,
				spacing = 6,
				layout = wibox.layout.fixed.horizontal,
			},
			{ -- MIDDLE: leerer Drag-Bereich (Spacer)
				nil,
				nil,
				nil,
				buttons = buttons,
				layout = wibox.layout.align.horizontal,
			},
			{ -- RIGHT: Buttons (minimize, floating toggle, close)
				awful.titlebar.widget.minimizebutton(c),
				awful.titlebar.widget.floatingbutton(c),
				awful.titlebar.widget.closebutton(c),
				spacing = 4,
				layout = wibox.layout.fixed.horizontal,
			},
			layout = wibox.layout.align.horizontal,
		})
end

-- Rahmen + runde Ecken (robust gegen 0×0-Geometrie)
function W.apply_client_style(c)
	if not (c and c.valid) then
		return
	end

	local is_max = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal
	local bw = tonumber(beautiful.border_width) or 0
	c.border_width = is_max and 0 or bw

	c.border_color = (c == client.focus) and beautiful.border_focus or beautiful.border_normal

	local r = tonumber(beautiful.border_radius) or 0

	-- Bei maximized, minimized oder r<=0: keine Shape-Funktion
	if is_max or c.minimized or r <= 0 then
		c.shape = nil
		return
	end

	-- Shape nur anwenden, wenn sinnvolle Abmessungen vorhanden sind
	c.shape = function(cr, w, h)
		-- Guard gegen 0/1-Pixel-Zustände beim Minimize/Floating-Toggle
		if (w or 0) < 2 or (h or 0) < 2 then
			return -- nichts zeichnen -> kein geometry-Fehler
		end
		gears.shape.rounded_rect(cr, w, h, r)
	end
end

-- Optionaler Convenience-Hook
function W.connect_signals()
	client.connect_signal("manage", W.apply_client_style)
	client.connect_signal("focus", W.apply_client_style)
	client.connect_signal("unfocus", W.apply_client_style)
	client.connect_signal("property::maximized", W.apply_client_style)
	client.connect_signal("property::fullscreen", W.apply_client_style)
	client.connect_signal("property::maximized_vertical", W.apply_client_style)
	client.connect_signal("property::maximized_horizontal", W.apply_client_style)
end

return W
