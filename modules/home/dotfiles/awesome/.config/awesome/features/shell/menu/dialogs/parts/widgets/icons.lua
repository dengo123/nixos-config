-- widgets/buttons/icons.lua
local awful, gears, wibox = require("awful"), require("gears"), require("wibox")

local I = {}
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

function I.mk_icon_button(args)
	args = args or {}
	local th, size = args.th or {}, args.size or 64
	local pad_icon, pad_cell, spacing = pick(th.icon_pad, 6), pick(th.icon_cell_pad, 6), pick(th.icon_spacing, 6)

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
		local emoji = args.emoji or "…"
		local font = args.emoji_font or ("sans " .. math.floor(size * 0.66))
		icon_inner = wibox.widget({
			markup = ("<span font='%s'>%s</span>"):format(font, emoji),
			align = "center",
			valign = "center",
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.textbox,
		})
	end

	local box_side = size + pad_icon * 2
	local square_shape = (th.icon_shape == "rounded")
			and function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, pick(th.icon_rounding, 6))
			end
		or gears.shape.rectangle

	local hover_square = wibox.widget({
		{
			{ icon_inner, halign = "center", valign = "center", widget = wibox.container.place },
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

	local label_block, label_h = nil, 0
	if args.label and #args.label > 0 then
		local fsz, leading = pick(th.icon_label_size, 18), pick(th.icon_label_leading, 1.25)
		label_h = math.ceil(fsz * leading)
		local lbl = wibox.widget({
			markup = ("<span font='sans %d' color='%s'>%s</span>"):format(
				fsz,
				th.icon_label_color or "#FFFFFF",
				args.label
			),
			align = "center",
			valign = "center",
			forced_height = label_h,
			widget = wibox.widget.textbox,
		})
		local label_w = box_side + (th.icon_cell_extra_w or 0)
		label_block = wibox.widget({ lbl, strategy = "exact", width = label_w, widget = wibox.container.constraint })
	end

	local v_spacing = (label_h > 0) and spacing or 0
	local square_centered = wibox.widget({
		{ hover_square, halign = "center", valign = "center", widget = wibox.container.place },
		top = math.floor((label_h + v_spacing) / 2),
		widget = wibox.container.margin,
	})

	local content =
		wibox.widget({ square_centered, label_block, spacing = spacing, layout = wibox.layout.fixed.vertical })
	local clickable = wibox.widget({
		content,
		left = pad_cell,
		right = pad_cell,
		top = pad_cell,
		bottom = pad_cell,
		widget = wibox.container.margin,
	})

	-- hover
	hover_square:connect_signal("mouse::enter", function()
		hover_square.bg = th.icon_hover_bg or "#FFFFFF22"
		hover_square.shape_border_width = pick(th.icon_hover_bw, 2)
		hover_square.shape_border_color = th.icon_hover_border or "#2B77FF"
	end)
	hover_square:connect_signal("mouse::leave", function()
		hover_square.bg = "#00000000"
		hover_square.shape_border_width = 0
	end)

	-- click
	if args.on_press then
		clickable:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	-- focus API
	function clickable:set_focus(on, th2)
		local t = th2 or th
		local bg_on = pick(t.icon_focus_bg, t.icon_hover_bg, "#FFFFFF22")
		local bw_on = pick(t.icon_focus_bw, t.icon_hover_bw, 2)
		local bor_on = pick(t.icon_focus_border, t.icon_hover_border, "#2B77FF")
		hover_square.bg = on and bg_on or "#00000000"
		hover_square.shape_border_width = on and bw_on or 0
		hover_square.shape_border_color = bor_on
	end

	function clickable:activate()
		if args.on_press then
			args.on_press()
		end
	end

	-- nützlich für Fokuslib
	clickable.mouse_enter_target = hover_square

	return clickable
end

return I
