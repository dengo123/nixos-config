-- ~/.config/awesome/shell/bar/widgets/start.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local gsurface = require("gears.surface")

local M = {}

function M.build(opts)
	opts = opts or {}
	local s = opts.screen or (mouse and mouse.screen) or nil
	local T = assert(opts.theme, "start widget: opts.theme (ui/theme/start.get(...)) fehlt")

	-- harte Theme-Prämissen
	assert(T.bg and T.bg_hover and T.fg and T.shape and T.margin, "start theme: basis fehlt")
	assert(T.icon and T.icon_size and T.spacing, "start theme: icon/icon_size/spacing fehlt")
	assert(T.font_size_scale and T.font_weight and T.font_style, "start theme: font props fehlen")
	assert(T.width_factor and T.fixed_height ~= nil and T.label, "start theme: width/fixed_height/label fehlt")

	local H = assert(tonumber(beautiful.wibar_height), "start widget: beautiful.wibar_height fehlt/ungueltig")
	local W = math.floor(T.width_factor * H)

	local bg, bg_hover, fg = T.bg, T.bg_hover, T.fg
	local shape = T.shape
	local margin = T.margin

	-- Icon laden
	local icon_path = T.icon
	if type(icon_path) == "string" and not icon_path:match("^/") then
		icon_path = gfs.get_configuration_dir() .. icon_path
	end
	local icon_surface = (type(icon_path) == "string") and gsurface.load_uncached(icon_path) or icon_path
	assert(icon_surface, "start theme: icon konnte nicht geladen werden")

	-- Innenhöhe = Barhöhe - vertikale Margins
	local inner_h = H - (margin.top + margin.bottom)
	local icon_px = math.min(T.icon_size, inner_h)

	local icon_widget = wibox.widget({
		image = icon_surface,
		resize = true,
		upscale = true, -- wichtig: erlaubt Vergrößern
		downscale = true,
		forced_width = icon_px,
		forced_height = icon_px,
		widget = wibox.widget.imagebox,
	})

	-- Label via Pango
	local label_markup = string.format(
		'<span weight="%s" style="%s" size="%d%%">%s</span>',
		T.font_weight,
		T.font_style,
		math.floor(100 * T.font_size_scale),
		T.label
	)

	local label_widget = wibox.widget({
		markup = label_markup,
		widget = wibox.widget.textbox,
	})

	local row_inner = wibox.widget({
		icon_widget,
		label_widget,
		spacing = T.spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	local row = wibox.widget({
		{
			row_inner,
			left = margin.left,
			right = margin.right,
			top = margin.top,
			bottom = margin.bottom,
			widget = wibox.container.margin,
		},
		fg = fg,
		widget = wibox.container.background,
	})

	local btn = wibox.widget({
		row,
		bg = bg,
		shape = shape,
		forced_width = W,
		forced_height = T.fixed_height and H or nil,
		widget = wibox.container.background,
	})

	-- Hover
	btn:connect_signal("mouse::enter", function()
		btn.bg = bg_hover
	end)
	btn:connect_signal("mouse::leave", function()
		btn.bg = bg
	end)

	-- Klick-Logik
	local launcher = opts.launcher -- system.config.launcher (nil | "rofi" | "emacs")
	local menu_api = opts.menu_api -- { show_for_widget = function(widget) ... }

	local function show_menu()
		if menu_api and menu_api.show_for_widget then
			menu_api.show_for_widget(btn)
			return true
		end
		return false
	end

	local function on_left_click()
		if launcher == nil or launcher == "" then
			-- nil → wie Rechtsklick: Menü öffnen
			if show_menu() then
				return
			end
			return
		end
		local L = tostring(launcher):lower()

		if L == "rofi" then
			awful.spawn.with_shell("rofi -show drun")
			return
		end

		if L == "emacs" then
			awful.spawn.with_shell("emacsclient -c || emacs")
			return
		end

		-- unbekannter Wert → sicherheitshalber Menü
		show_menu()
	end

	btn:buttons(gears.table.join(
		awful.button({}, 1, on_left_click), -- Links: nil/rofi/emacs
		awful.button({}, 3, function()
			show_menu()
		end) -- Rechts: Menü
	))

	return btn
end

return M
