-- ~/.config/awesome/features/shell/menu/dialogs/parts/widgets.lua
-- UI-Bausteine (Header, Footer, Icon). Reines Layout/Verhalten.
-- Sämtliche Styles kommen aus theme.lua.

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local Theme = require("features.shell.menu.dialogs.parts.theme")

local W = {}

-- ======================================================================
-- Header
-- ======================================================================
function W.mk_header(title, th)
	th = th or Theme.defaults

	local icon_widget
	if th.header_icon_path and #th.header_icon_path > 0 then
		icon_widget = wibox.widget({
			image = th.header_icon_path,
			resize = true,
			forced_width = th.header_icon_size or 20,
			forced_height = th.header_icon_size or 20,
			widget = wibox.widget.imagebox,
		})
	else
		icon_widget = wibox.widget({
			markup = string.format(
				"<span font='sans %d'>%s</span>",
				th.header_icon_size or th.header_font_size or 14,
				th.header_icon or ""
			),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	local text_widget = wibox.widget({
		markup = string.format("<span font='sans %d'><b>%s</b></span>", th.header_font_size or 14, title or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	return wibox.widget({
		{
			icon_widget,
			text_widget,
			spacing = th.pad_h or 8,
			layout = wibox.layout.fixed.horizontal,
		},
		left = th.pad_h,
		right = th.pad_h,
		widget = wibox.container.margin,
		bg = th.header_bg,
		fg = th.header_fg,
		widget = wibox.container.background,
	})
end

-- ======================================================================
-- Footer (Cancel rechts, Hover = Border; Höhe NICHT fix → kein Clipping)
-- ======================================================================
function W.mk_footer(cancel_label, on_cancel, th)
	th = th or Theme.defaults

	local inner = wibox.widget({
		{
			{
				text = cancel_label or "Cancel",
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = th.cancel_pad_h,
			right = th.cancel_pad_h,
			top = th.cancel_pad_v,
			bottom = th.cancel_pad_v,
			widget = wibox.container.margin,
		},
		bg = th.cancel_bg,
		fg = th.cancel_fg,
		shape = function(cr, w, h)
			if th.cancel_radius > 0 then
				gears.shape.rounded_rect(cr, w, h, th.cancel_radius)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})

	local dialog_w = th._computed_dialog_w or 0
	local target_w = dialog_w > 0 and math.floor(dialog_w / 5) or nil

	local cancel_btn = wibox.widget({
		inner,
		strategy = "exact",
		width = target_w, -- nur Breite per Ratio; Höhe passt sich Inhalt an
		widget = wibox.container.constraint,
	})

	cancel_btn:connect_signal("mouse::enter", function()
		inner.bg = th.cancel_hover_bg
		inner.shape_border_width = th.cancel_hover_bw
		inner.shape_border_color = th.cancel_hover_border
	end)
	cancel_btn:connect_signal("mouse::leave", function()
		inner.bg = th.cancel_bg
		inner.shape_border_width = 0
		inner.shape_border_color = "#00000000"
	end)
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		if on_cancel then
			on_cancel()
		end
	end)))

	local right_cell = wibox.widget({
		{
			cancel_btn,
			halign = "right",
			valign = "center",
			widget = wibox.container.place,
		},
		right = th.pad_h,
		widget = wibox.container.margin,
	})

	return wibox.widget({
		{ right_cell, top = 8, bottom = 8, widget = wibox.container.margin },
		bg = th.footer_bg,
		fg = th.footer_fg,
		widget = wibox.container.background,
	})
end

-- ======================================================================
-- Icon-Button (quadratisches Icon mit Hover-Rahmen, Label drunter)
-- args: { size, icon|emoji, emoji_font?, label?, on_press?, th? }
-- ======================================================================
function W.mk_icon(args)
	local th = (args and args.th) or Theme.defaults
	local size = (args and args.size) or 64

	local pad_icon = th.icon_pad
	local pad_cell = th.icon_cell_pad
	local spacing = th.icon_spacing
	local cell_extraW = th.icon_cell_extra_w

	-- -------- Icon (Bild oder Emoji) ------------------------------------
	local icon_img
	if args and args.icon and type(args.icon) == "string" then
		icon_img = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	else
		icon_img = wibox.widget({
			markup = string.format(
				"<span font='%s'>%s</span>",
				(args and args.emoji_font) or ("sans " .. math.floor(size * 0.66)),
				(args and args.emoji) or "⏻"
			),
			align = "center",
			valign = "center",
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.textbox,
		})
	end

	-- -------- Icon-Quadrat (Hover-Target) -------------------------------
	local icon_shape = (
		th.icon_shape == "rounded"
		and function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, th.icon_rounding)
		end
	) or gears.shape.rectangle

	local icon_bg = wibox.widget({
		{
			icon_img,
			margins = pad_icon,
			widget = wibox.container.margin,
		},
		forced_width = size + pad_icon * 2,
		forced_height = size + pad_icon * 2,
		shape = icon_shape,
		bg = "#00000000",
		widget = wibox.container.background,
	})

	-- -------- Label (einzeilig, ellipsize) -------------------------------
	local label_widget, label_h
	if args and args.label and #args.label > 0 then
		local lines = th.icon_label_lines
		local fsize = th.icon_label_size
		local leading = th.icon_label_leading
		label_h = math.floor(fsize * leading * lines)

		label_widget = wibox.widget({
			markup = string.format("<span font='sans %d' color='%s'>%s</span>", fsize, th.icon_label_color, args.label),
			align = "center",
			valign = "center",
			wrap = "none", -- kein Umbruch
			ellipsize = "end", -- … bei Platzmangel
			forced_height = label_h,
			widget = wibox.widget.textbox,
		})
	else
		label_h = 0
	end

	-- -------- Gesamthöhe der Zelle FIX berechnen ------------------------
	local icon_block_h = size + pad_icon * 2
	local v_spacing = (label_h > 0) and spacing or 0
	local CELL_H = pad_cell + icon_block_h + v_spacing + label_h + pad_cell

	-- -------- Clickable-Cell: Breite & Höhe fix -------------------------
	local clickable = wibox.widget({
		{
			{
				icon_bg,
				label_widget,
				spacing = spacing,
				layout = wibox.layout.fixed.vertical,
			},
			margins = pad_cell,
			widget = wibox.container.margin,
		},
		strategy = "exact",
		width = size + pad_icon * 2 + pad_cell * 2 + cell_extraW,
		height = CELL_H,
		widget = wibox.container.constraint,
	})

	if args and args.on_press then
		clickable:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	-- -------- Hover nur am Icon-Quadrat --------------------------------
	icon_bg:connect_signal("mouse::enter", function()
		icon_bg.bg = th.icon_hover_bg
		icon_bg.shape_border_width = th.icon_hover_bw
		icon_bg.shape_border_color = th.icon_hover_border
	end)
	icon_bg:connect_signal("mouse::leave", function()
		icon_bg.bg = "#00000000"
		icon_bg.shape_border_width = 0
	end)

	return clickable
end

return W
