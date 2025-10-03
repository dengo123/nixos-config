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

	-- Innenhöhe (Barhöhe minus Margins)
	local inner_h = H - (margin.top + margin.bottom)

	-- ENTWEDER: feste Pixelgröße, aber max. Innenhöhe
	local icon_px = math.min(T.icon_size, inner_h)

	-- OPTIONAL (Alternative): relative Skalierung
	-- local icon_scale = T.icon_scale or 0.95    -- 0..1
	-- local icon_px    = math.floor(icon_scale * inner_h)

	local icon_widget = wibox.widget({
		image = icon_surface,
		resize = true,
		upscale = true, -- <<<<<< WICHTIG: erlaubt Vergrößern über native Größe
		downscale = true, -- optional, erlaubt Verkleinern (meist default)
		forced_width = icon_px,
		forced_height = icon_px,
		widget = wibox.widget.imagebox,
	})

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

	btn:connect_signal("mouse::enter", function()
		btn.bg = bg_hover
	end)
	btn:connect_signal("mouse::leave", function()
		btn.bg = bg
	end)

	-- Klick-Logik (wie vereinbart: nil → Menü wie Rechtsklick)
	local launcher = opts.launcher
	local terminal = opts.terminal or "xterm"
	local menu = opts.menu
	local menu_api = opts.menu_api

	local function show_menu()
		if menu_api and menu_api.show_for_widget then
			menu_api.show_for_widget(btn)
			return true
		end
		return false
	end

	local function run_launcher()
		if type(launcher) ~= "string" or launcher == "" then
			if show_menu() then
				return
			end
			awful.spawn.with_shell(terminal)
			return
		end
		local L = launcher:lower()
		if L == "awesome" then
			if menu and menu.toggle then
				menu:toggle({ screen = s })
			elseif menu and menu.show then
				menu:show({ screen = s })
			else
				awful.spawn.with_shell(terminal)
			end
			return
		end
		if L == "rofi" then
			awful.spawn.with_shell("rofi -show drun")
			return
		end
		if L == "emacs" then
			awful.spawn.with_shell("emacsclient -c || emacs")
			return
		end
		awful.spawn.with_shell(launcher)
	end

	btn:buttons(gears.table.join(
		awful.button({}, 1, run_launcher),
		awful.button({}, 3, function()
			show_menu()
		end)
	))

	return btn
end

return M
