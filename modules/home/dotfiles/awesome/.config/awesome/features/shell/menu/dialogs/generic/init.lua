-- features/shell/menu/dialogs/form.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Theme = require("features.shell.menu.dialogs.parts.theme")
local Popup = require("features.shell.menu.dialogs.parts.popup")
local WParts = require("features.shell.menu.dialogs.parts.widgets")

local Form = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- kleiner Text-Header (links Titel, rechts Close)
local function mk_header(title, th, on_close)
	local titlebox = wibox.widget({
		markup = string.format("<span font='sans %d'><b>%s</b></span>", pick(th.header_font_size, 12), title or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local close_th = Theme.merge(th, { cancel_pad_h = 8, cancel_pad_v = 2, cancel_radius = 2 })
	local close_btn = WParts.mk_cancel_button("×", on_close, close_th)

	local bar = wibox.widget({
		{
			{ titlebox, left = pick(th.header_pad_h, th.pad_h, 12), widget = wibox.container.margin },
			{
				close_btn,
				halign = "right",
				valign = "center",
				widget = wibox.container.place,
			},
			layout = wibox.layout.align.horizontal,
		},
		bg = pick(th.header_bg, "#ECE9D8"),
		fg = pick(th.header_fg, "#000000"),
		widget = wibox.container.background,
	})

	-- fixierte Headerhöhe (28px default)
	return wibox.widget({
		bar,
		strategy = "exact",
		height = pick(th.header_h, 28),
		widget = wibox.container.constraint,
	})
end

-- Button aus Cancel-Stil ableiten (überschreibbar pro Button)
local function mk_button(def, th)
	local th_btn = Theme.merge(th, def.theme_override or {})
	return WParts.mk_cancel_button(def.label or "OK", def.on_press, th_btn)
end

-- Footer = gleiche Farben wie Body/Client
local function mk_footer(buttons, th, align)
	local row = wibox.layout.fixed.horizontal()
	row.spacing = pick(th.footer_btn_spacing, 8)
	for _, b in ipairs(buttons or {}) do
		row:add(mk_button(b, th))
	end

	local placed = wibox.widget({
		row,
		halign = (align == "center") and "center" or "right",
		valign = "center",
		widget = wibox.container.place,
	})

	return wibox.widget({
		{
			placed,
			left = pick(th.pad_h, 12),
			right = pick(th.pad_h, 12),
			top = pick(th.footer_pad_v, 8),
			bottom = pick(th.footer_pad_v, 8),
			widget = wibox.container.margin,
		},
		-- << hier Body-Farben nutzen
		bg = pick(th.body_bg, "#FFFFFF"),
		fg = pick(th.body_fg, "#000000"),
		widget = wibox.container.background,
	})
end

-- API: Form.open{ title, content=widget, width, height, buttons={}, align="right", theme={} }
function Form.open(opts)
	opts = opts or {}
	local th = Theme.get(opts.theme or {})

	local W = opts.width or pick(th.dialog_w, 520)
	local H = opts.height or pick(th.dialog_h, 360)

	local handle
	local function close()
		if handle and handle.close then
			handle.close()
		end
	end

	local header = mk_header(opts.title or "", th, close)

	local body_inner = wibox.widget({
		opts.content or wibox.widget.textbox("No content"),
		left = pick(th.pad_h, 12),
		right = pick(th.pad_h, 12),
		top = pick(th.pad_v, 10),
		bottom = pick(th.pad_v, 10),
		widget = wibox.container.margin,
	})
	local body = wibox.widget({
		body_inner,
		bg = pick(th.body_bg, "#FFFFFF"),
		fg = pick(th.body_fg, "#000000"),
		widget = wibox.container.background,
	})

	local footer = mk_footer(opts.buttons or {
		{ label = "OK", on_press = close },
		{ label = "Cancel", on_press = close },
	}, th, opts.align or "right")

	local root = wibox.widget({
		header,
		body,
		footer,
		layout = wibox.layout.fixed.vertical,
	})

	-- Border 2px in Headerfarbe, runde Ecken
	handle = Popup.show(
		root,
		Theme.merge(th, {
			dialog_radius = pick(th.dialog_radius, 6),
			dialog_border_width = pick(th.dialog_border_width, 2),
			dialog_border = pick(th.dialog_border, th.header_bg or "#3A6EA5"),
			dialog_bg = pick(th.dialog_bg, th.body_bg or "#FFFFFF"),
		}),
		{ width = W, height = H }
	)

	return handle
end

return Form
