-- Atomare UI-Bausteine (ohne Container-Logik):
--  - mk_header_content(title, th)
--  - mk_cancel_button(label, on_click, th)
--  - mk_icon_button{ icon|emoji, size, label, on_press, th }

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- ======================================================================
-- Header-Inhalt (Icon + Titel) OHNE Container/Farbe/Margins
-- ======================================================================
function W.mk_header_content(title, th)
	th = th or {}

	local icon_widget
	if th.header_icon_path and #th.header_icon_path > 0 then
		icon_widget = wibox.widget({
			image = th.header_icon_path,
			resize = true,
			forced_width = pick(th.header_icon_size, th.header_font_size, 20),
			forced_height = pick(th.header_icon_size, th.header_font_size, 20),
			widget = wibox.widget.imagebox,
		})
	else
		icon_widget = wibox.widget({
			markup = string.format(
				"<span font='sans %d'>%s</span>",
				pick(th.header_icon_size, th.header_font_size, 14),
				th.header_icon or ""
			),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	local text_widget = wibox.widget({
		markup = string.format("<span font='sans %d'><b>%s</b></span>", pick(th.header_font_size, 14), title or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	return wibox.widget({
		icon_widget,
		text_widget,
		spacing = pick(th.header_spacing, th.pad_h, 8),
		layout = wibox.layout.fixed.horizontal,
	})
end

-- ======================================================================
-- Cancel-Button (Button + Hover/Borders). Events sitzen auf äußerstem Wrapper.
-- ======================================================================
function W.mk_cancel_button(label, on_click, th)
	th = th or {}

	local inner = wibox.widget({
		{
			{
				text = label or "Cancel",
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = (th.cancel_pad_h or 12),
			right = (th.cancel_pad_h or 12),
			top = (th.cancel_pad_v or 8),
			bottom = (th.cancel_pad_v or 8),
			widget = wibox.container.margin,
		},
		bg = th.cancel_bg or "#334155",
		fg = th.cancel_fg or "#FFFFFF",
		shape = function(cr, w, h)
			local r = (th.cancel_radius or 8)
			if r > 0 then
				gears.shape.rounded_rect(cr, w, h, r)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})

	local root = wibox.widget({
		inner,
		widget = wibox.container.background,
	})

	root:connect_signal("mouse::enter", function()
		inner.bg = th.cancel_hover_bg or inner.bg
		inner.shape_border_width = th.cancel_hover_bw or 2
		inner.shape_border_color = th.cancel_hover_border or "#2B77FF"
	end)
	root:connect_signal("mouse::leave", function()
		inner.bg = th.cancel_bg or "#334155"
		inner.shape_border_width = 0
		inner.shape_border_color = "#00000000"
	end)

	root:buttons(gears.table.join(awful.button({}, 1, function()
		if on_click then
			on_click()
		end
	end)))

	return root
end

-- ======================================================================
-- Icon-Button (quadratisches Icon + optionales Label, Hover nur am Icon)
-- ======================================================================
function W.mk_icon_button(args)
	args = args or {}
	local th = args.th or {}
	local size = args.size or 64

	local pad_icon = pick(th.icon_pad, 6)
	local pad_cell = pick(th.icon_cell_pad, 6)
	local spacing = pick(th.icon_spacing, 6)

	-- Inneres Icon
	local icon_inner
	if args.icon and type(args.icon) == "string" then
		icon_inner = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	else
		local emoji_char = args.emoji or "…"
		local emoji_font = args.emoji_font or ("sans " .. math.floor(size * 0.66))
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
			gears.shape.rounded_rect(cr, w, h, pick(th.icon_rounding, 6))
		end
	) or gears.shape.rectangle

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

	-- Label (optional)
	local label_block, label_h = nil, 0
	if args.label and #args.label > 0 then
		local fsz = pick(th.icon_label_size, 18)
		local leading = pick(th.icon_label_leading, 1.25)
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

	local v_spacing = (label_h > 0) and spacing or 0
	local offset = math.floor((label_h + v_spacing) / 2)

	local square_centered = wibox.widget({
		{
			hover_square,
			halign = "center",
			valign = "center",
			widget = wibox.container.place,
		},
		top = offset,
		widget = wibox.container.margin,
	})

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
		hover_square.shape_border_width = pick(th.icon_hover_bw, 2)
		hover_square.shape_border_color = th.icon_hover_border or "#2B77FF"
	end)
	hover_square:connect_signal("mouse::leave", function()
		hover_square.bg = "#00000000"
		hover_square.shape_border_width = 0
	end)

	if args.on_press then
		clickable:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	return clickable
end

return W
