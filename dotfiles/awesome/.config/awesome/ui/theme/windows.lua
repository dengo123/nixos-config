-- ~/.config/awesome/ui/theme/windows.lua
local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local W = {}

-- =========================
-- Feste Vorgaben (Größen/Position)
-- =========================
local BORDER_WIDTH = 2
local BORDER_RADIUS = 10
local TITLEBAR_POS = "top"
local TITLEBAR_HEIGHT = 28

-- =========================
-- Titlebar-Assets lokal absichern (nur für windows.lua)
-- =========================
local function ensure_titlebar_assets()
	if
		beautiful.titlebar_close_button_normal
		or beautiful.titlebar_minimize_button_normal
		or beautiful.titlebar_floating_button_normal
		or beautiful.titlebar_maximized_button_normal
	then
		return
	end
	local p = gears.filesystem.get_themes_dir() .. "default/theme.lua"
	local ok, def = pcall(dofile, p)
	if not ok or type(def) ~= "table" then
		return
	end

	local keys = {
		"titlebar_close_button_normal",
		"titlebar_close_button_focus",
		"titlebar_minimize_button_normal",
		"titlebar_minimize_button_focus",
		"titlebar_floating_button_normal_active",
		"titlebar_floating_button_focus_active",
		"titlebar_floating_button_normal_inactive",
		"titlebar_floating_button_focus_inactive",
		"titlebar_maximized_button_normal_active",
		"titlebar_maximized_button_focus_active",
		"titlebar_maximized_button_normal_inactive",
		"titlebar_maximized_button_focus_inactive",
	}
	for _, k in ipairs(keys) do
		if beautiful[k] == nil and def[k] ~= nil then
			beautiful[k] = def[k]
		end
	end
end

-- =========================
-- Button-Factory (Bildbutton mit Hover-Recolor)
-- =========================
local function make_img_button(opts)
	-- opts = { img, color, color_hover, size, on_click }
	local bar_h = opts.size or (beautiful.titlebar_height or TITLEBAR_HEIGHT)
	local inner = math.floor(bar_h * 0.90) -- 90% der Titlebar-Höhe
	local pad = math.floor((bar_h - inner) / 2)

	local ib = wibox.widget({
		image = gears.color.recolor_image(opts.img, opts.color),
		resize = true,
		forced_width = inner,
		forced_height = inner,
		widget = wibox.widget.imagebox,
	})

	local bg = wibox.widget({
		{ ib, left = pad, right = pad, top = pad, bottom = pad, widget = wibox.container.margin },
		widget = wibox.container.background,
	})

	bg:connect_signal("mouse::enter", function()
		ib.image = gears.color.recolor_image(opts.img, opts.color_hover or opts.color)
	end)
	bg:connect_signal("mouse::leave", function()
		ib.image = gears.color.recolor_image(opts.img, opts.color)
	end)
	bg:buttons(gears.table.join(awful.button({}, 1, function()
		if type(opts.on_click) == "function" then
			opts.on_click()
		end
	end)))
	return bg
end

-- =========================
-- Init: setzt beautiful.* HART (Farben aus Palette)
-- =========================
function W.init(cfg)
	ensure_titlebar_assets()

	local C = (cfg and cfg.colors) or require("ui.colors").get()

	-- Rahmen / Farben
	beautiful.border_width = BORDER_WIDTH
	beautiful.border_radius = BORDER_RADIUS
	beautiful.border_normal = C.blue_light
	beautiful.border_focus = C.blue_luna

	-- Titlebar
	beautiful.titlebar_position = TITLEBAR_POS
	beautiful.titlebar_height = TITLEBAR_HEIGHT
	beautiful.titlebar_bg_normal = C.blue_light
	beautiful.titlebar_bg_focus = C.blue_luna
	beautiful.titlebar_fg_normal = C.gray -- einheitlicher Grauton
	beautiful.titlebar_fg_focus = C.white
end

-- =========================
-- Titlebar anheften (eigene Buttons)
-- =========================
function W.attach_titlebar(c, mouse, _opts)
	if not (c and c.valid) then
		return
	end

	local C = (awesome and _G and _G.ui_cfg and _G.ui_cfg.colors) or require("ui.colors").get()

	local pos = beautiful.titlebar_position or TITLEBAR_POS
	local size = beautiful.titlebar_height or TITLEBAR_HEIGHT
	local buttons = (mouse and mouse.titlebar_buttons and mouse.titlebar_buttons(c)) or nil

	local title = awful.titlebar.widget.titlewidget(c)
	if title.set_align then
		title:set_align("left")
	end

	-- Assets aus beautiful
	local img_close = beautiful.titlebar_close_button_normal
	local img_minimize = beautiful.titlebar_minimize_button_normal
	local img_float = beautiful.titlebar_floating_button_normal_active
		or beautiful.titlebar_floating_button_normal_inactive
		or beautiful.titlebar_floating_button_normal

	-- Buttons mit Palettenfarben
	local btn_min = make_img_button({
		img = img_minimize,
		color = C.white,
		color_hover = C.gray, -- ausgrauen
		size = size,
		on_click = function()
			c.minimized = true
		end,
	})

	local btn_float = make_img_button({
		img = img_float,
		color = C.white,
		color_hover = C.gray,
		size = size,
		on_click = function()
			awful.client.floating.toggle(c)
		end,
	})

	local btn_close = make_img_button({
		img = img_close,
		color = C.red, -- vorher "pink", jetzt rot
		color_hover = C.pink, -- helleres Rosa
		size = size,
		on_click = function()
			c:kill()
		end,
	})

	awful.titlebar(c, { position = pos, size = size }):setup({
		{
			awful.titlebar.widget.iconwidget(c),
			title,
			buttons = buttons,
			spacing = 6,
			layout = wibox.layout.fixed.horizontal,
		},
		{
			nil,
			nil,
			nil,
			buttons = buttons,
			layout = wibox.layout.align.horizontal,
		},
		{
			btn_min,
			btn_float,
			btn_close,
			spacing = 4,
			layout = wibox.layout.fixed.horizontal,
		},
		layout = wibox.layout.align.horizontal,
	})
end

-- =========================
-- Rahmen/Shape anwenden
-- =========================
function W.apply_client_style(c)
	if not (c and c.valid) then
		return
	end

	local is_max = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal
	c.border_width = is_max and 0 or (beautiful.border_width or BORDER_WIDTH)
	c.border_color = (c == client.focus) and (beautiful.border_focus or beautiful.border_normal)
		or beautiful.border_normal

	local r = beautiful.border_radius or BORDER_RADIUS
	if is_max or c.minimized or r <= 0 then
		c.shape = nil
		return
	end
	c.shape = function(cr, w, h)
		if (w or 0) < 2 or (h or 0) < 2 then
			return
		end
		gears.shape.rounded_rect(cr, w, h, r)
	end
end

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
