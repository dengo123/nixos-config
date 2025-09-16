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
	local target_w = th.cancel_width or (dialog_w > 0 and math.floor(dialog_w / 7) or nil)

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

	local pad_icon = th.icon_pad or 6
	local pad_cell = th.icon_cell_pad or 6
	local spacing = th.icon_spacing or 6

	-- Icon (Bild oder Emoji), strikt quadratisch
	local icon_inner
	if args and args.icon and type(args.icon) == "string" then
		icon_inner = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	else
		local emoji_char = (args and args.emoji) or "..."
		local emoji_font = (args and args.emoji_font) or ("sans " .. math.floor(size * 0.66))
		icon_inner = wibox.widget({
			markup = string.format("<span font='%s'>%s</span>", emoji_font, emoji_char),
			align = "center",
			valign = "center",
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.textbox,
		})
	end

	local box_side = size + pad_icon * 2
	local square_shape = (
		th.icon_shape == "rounded"
		and function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, th.icon_rounding or 6)
		end
	) or gears.shape.rectangle

	-- Hover-Quadrat (klein, strikt 1:1)
	local hover_square = wibox.widget({
		{
			{
				icon_inner,
				halign = "center",
				valign = "center",
				widget = wibox.container.place,
			},
			margins = pad_icon,
			widget = wibox.container.margin,
		},
		forced_width = box_side,
		forced_height = box_side,
		shape = square_shape,
		shape_clip = true,
		bg = "#00000000",
		widget = wibox.container.background,
	})

	-- Label (einzeilig, Breite = Quadrat + Extra)
	local label_block, label_h = nil, 0
	if args and args.label and #args.label > 0 then
		local fsz = th.icon_label_size or 18
		local leading = th.icon_label_leading or 1.25
		label_h = math.ceil(fsz * leading)

		local lbl = wibox.widget({
			markup = string.format(
				"<span font='sans %d' color='%s'>%s</span>",
				fsz,
				th.icon_label_color or "#FFFFFF",
				args.label
			),
			align = "center",
			valign = "center",
			wrap = "none",
			ellipsize = "none",
			forced_height = label_h,
			widget = wibox.widget.textbox,
		})

		local label_w = box_side + (th.icon_cell_extra_w or 0)
		label_block = wibox.widget({
			lbl,
			strategy = "exact",
			width = label_w,
			widget = wibox.container.constraint,
		})
	end

	-- Wahrnehmbar zentrieren: Quadrat um die halbe Label-Zone nach unten schieben
	local v_spacing = (label_h > 0) and spacing or 0
	local offset = math.floor((label_h + v_spacing) / 2)

	local square_centered = wibox.widget({
		{
			hover_square,
			halign = "center",
			valign = "center",
			widget = wibox.container.place,
		},
		top = offset, -- <<— schiebt das Quadrat nach unten
		widget = wibox.container.margin,
	})

	-- Gesamthöhe der Zelle (nur zur Info; base.lua regelt Breite)
	local icon_block_h = box_side
	local CELL_H = pad_cell + icon_block_h + v_spacing + label_h + pad_cell

	-- Vertikale Anordnung: versetztes Quadrat + Label
	local content = wibox.widget({
		square_centered,
		label_block,
		spacing = spacing,
		layout = wibox.layout.fixed.vertical,
	})

	local clickable = wibox.widget({
		content,
		left = pad_cell,
		right = pad_cell,
		top = pad_cell,
		bottom = pad_cell,
		widget = wibox.container.margin,
	})

	-- Hover nur im Quadrat
	hover_square:connect_signal("mouse::enter", function()
		hover_square.bg = th.icon_hover_bg or "#FFFFFF22"
		hover_square.shape_border_width = th.icon_hover_bw or 2
		hover_square.shape_border_color = th.icon_hover_border or "#2B77FF"
	end)
	hover_square:connect_signal("mouse::leave", function()
		hover_square.bg = "#00000000"
		hover_square.shape_border_width = 0
	end)

	-- Klick
	if args and args.on_press then
		clickable:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	return clickable
end

return W
