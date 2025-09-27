-- widgets/buttons/cancel.lua
local awful, gears, wibox = require("awful"), require("gears"), require("wibox")
local Icons = require("features.shell.menu.dialogs.parts.widgets.buttons.icons")

local C = {}
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- 1) Pill-Variante (klassisch)
function C.mk_cancel_pill(label, on_click, th)
	th = th or {}
	local inner = wibox.widget({
		{
			{ text = label or "Cancel", align = "center", valign = "center", widget = wibox.widget.textbox },
			left = th.cancel_pad_h or 12,
			right = th.cancel_pad_h or 12,
			top = th.cancel_pad_v or 8,
			bottom = th.cancel_pad_v or 8,
			widget = wibox.container.margin,
		},
		bg = th.cancel_bg or "#334155",
		fg = th.cancel_fg or "#FFFFFF",
		shape = function(cr, w, h)
			local r = th.cancel_radius or 8
			if r > 0 then
				gears.shape.rounded_rect(cr, w, h, r)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		widget = wibox.container.background,
	})
	local root = wibox.widget({ inner, widget = wibox.container.background })
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

	-- Fokus-API fallback (für Konsistenz)
	function root:set_focus(on, th2)
		local t = th2 or th
		local on_bg = pick(t.cancel_bg_hover, t.row_bg_hover, t.bg_focus, "#FFFFFF22")
		local off_bg = pick(t.cancel_bg, "#00000000")
		self.bg = on and on_bg or off_bg
	end

	function root:activate()
		if on_click then
			on_click()
		end
	end

	root.mouse_enter_target = root
	return root
end

-- 2) Icon-like Cancel (für panel): nutzt mk_icon_button, aber eigene Farben/Defaults
function C.mk_cancel_icon(args)
	args = args or {}
	local th = args.th or {}

	-- Theme remapping: eigene Cancel-Farben > sonst Icon-Farben
	local th2 = setmetatable({
		icon_hover_bg = th.cancel_hover_bg or th.icon_hover_bg,
		icon_hover_bw = th.cancel_hover_bw or th.icon_hover_bw,
		icon_hover_border = th.cancel_hover_border or th.icon_hover_border,
		icon_focus_bg = th.cancel_focus_bg or th.icon_focus_bg,
		icon_focus_bw = th.cancel_focus_bw or th.icon_focus_bw,
		icon_focus_border = th.cancel_focus_border or th.icon_focus_border,
		icon_shape = th.icon_shape,
		icon_rounding = th.icon_rounding,
		icon_label_size = th.icon_label_size,
		icon_label_color = th.icon_label_color,
		icon_pad = th.icon_pad,
		icon_cell_pad = th.icon_cell_pad,
		icon_spacing = th.icon_spacing,
		icon_cell_extra_w = th.icon_cell_extra_w,
	}, { __index = th })

	local button = Icons.mk_icon_button({
		emoji = args.emoji or "✖",
		icon = args.icon,
		label = args.label or pick(th.cancel_label, "Cancel"),
		size = args.size,
		th = th2,
		on_press = args.on_press, -- kann später durch :activate ersetzt werden
	})

	return button
end

return C
