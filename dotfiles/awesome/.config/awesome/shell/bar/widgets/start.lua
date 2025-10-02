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

	-- Harte Theme-Prämissen (keine Fallbacks hier!)
	assert(T.bg, "start theme: bg fehlt")
	assert(T.bg_hover, "start theme: bg_hover fehlt")
	assert(T.fg, "start theme: fg fehlt")
	assert(T.shape, "start theme: shape fehlt")
	assert(T.margin, "start theme: margin fehlt")
	assert(T.icon, "start theme: icon fehlt")
	assert(T.icon_size, "start theme: icon_size fehlt")
	assert(T.spacing, "start theme: spacing fehlt")
	assert(T.font_size_scale, "start theme: font_size_scale fehlt")
	assert(T.font_weight, "start theme: font_weight fehlt")
	assert(T.font_style, "start theme: font_style fehlt")
	assert(T.width_factor, "start theme: width_factor fehlt")
	assert(T.fixed_height ~= nil, "start theme: fixed_height fehlt (true/false)")
	assert(T.label, "start theme: label fehlt")

	local H = assert(tonumber(beautiful.wibar_height), "beautiful.wibar_height fehlt/ungueltig")
	local W = math.floor(T.width_factor * H)

	-- Farben/Shape/Margins
	local bg, bg_hover, fg = T.bg, T.bg_hover, T.fg
	local shape = T.shape
	local margin = T.margin

	-- Icon laden (Theme liefert Pfad oder Surface)
	local icon_path = T.icon
	if type(icon_path) == "string" and not icon_path:match("^/") then
		icon_path = gfs.get_configuration_dir() .. icon_path
	end
	local icon_surface = (type(icon_path) == "string") and gsurface.load_uncached(icon_path) or icon_path
	assert(icon_surface, "start theme: icon konnte nicht geladen werden")

	local icon_widget = wibox.widget({
		image = icon_surface,
		resize = true,
		forced_width = T.icon_size,
		forced_height = T.icon_size,
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
	local launcher = opts.launcher
	local terminal = opts.terminal or "xterm"
	local menu = opts.menu
	local menu_api = opts.menu_api -- erwartet: { show_for_widget = function(widget) ... , get_start_items = function() ... end }

	local function run_launcher()
		if type(launcher) ~= "string" or launcher == "" then
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

	-- Buttons: Left = Launcher/Terminal, Right = shell/menu Dropdown
	btn:buttons(gears.table.join(
		awful.button({}, 1, run_launcher),
		awful.button({}, 3, function()
			if menu_api and menu_api.show_for_widget then
				-- Menü-Positionierung macht shell/menu: linksbündig über dem Button
				menu_api.show_for_widget(btn)
			end
		end)
	))

	return btn
end

return M
