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
	local T = assert(opts.theme, "start widget needs opts.theme from ui/theme/start.get(cfg.start)")

	local H = tonumber(beautiful.wibar_height) or 28
	local W = math.floor((T.width_factor or 4) * H)

	-- Farben/Shape/Margins
	local bg, bg_hover, fg = T.bg, (T.bg_hover or T.bg), T.fg
	local shape = T.shape or gears.shape.rounded_rect
	local margin = T.margin or { left = 16, right = 16, top = 4, bottom = 4 }

	-- Icon robust laden + feste Größe
	local icon_path = T.icon
	if type(icon_path) == "string" and not icon_path:match("^/") then
		icon_path = gfs.get_configuration_dir() .. icon_path
	end
	local icon_surface = (type(icon_path) == "string") and gsurface.load_uncached(icon_path) or icon_path
	if not icon_surface then
		icon_surface = gsurface.load_uncached(gfs.get_themes_dir() .. "default/icon.png")
	end
	local icon_size = T.icon_size or math.floor(H * 0.9)
	local icon_widget = wibox.widget({
		image = icon_surface,
		resize = true,
		forced_width = icon_size,
		forced_height = icon_size,
		widget = wibox.widget.imagebox,
	})

	-- Label: größer + fett + kursiv via Pango
	local scale = T.font_size_scale or 1.75
	local weight = T.font_weight or "bold"
	local style = T.font_style or "italic"
	local label_text = T.label or "start"
	local label_markup = string.format(
		'<span weight="%s" style="%s" size="%d%%">%s</span>',
		weight,
		style,
		math.floor(100 * scale),
		label_text
	)
	local label_widget = wibox.widget({
		markup = label_markup,
		widget = wibox.widget.textbox,
	})

	local row_inner = wibox.widget({
		icon_widget,
		label_widget,
		spacing = T.spacing or 14, -- Abstand Icon ↔ Text
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
		forced_width = W, -- z. B. 5× Wibar-Höhe
		forced_height = (T.fixed_height ~= false) and H or nil,
		widget = wibox.container.background,
	})

	btn:connect_signal("mouse::enter", function()
		if bg_hover then
			btn.bg = bg_hover
		end
	end)
	btn:connect_signal("mouse::leave", function()
		if bg then
			btn.bg = bg
		end
	end)

	-- Klick-Logik
	local launcher = opts.launcher
	local terminal = opts.terminal or "xterm"
	local menu = opts.menu
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
	btn:buttons(gears.table.join(awful.button({}, 1, run_launcher)))

	return btn
end

return M
