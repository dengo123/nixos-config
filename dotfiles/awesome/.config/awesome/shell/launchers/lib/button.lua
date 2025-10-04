-- ~/.config/awesome/shell/launchers/lib/button.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local B = {}

local STYLE = {
	height = 24,
	min_width = 96,
	radius = 4,
	pad_h = 14,
	font = "Sans 11",
	bg = "#F5F5EE",
	fg = "#111111",
	hover_bg = "#EDEDE6",
	border = "#2B77FF",
	border_bw = 3,
}

function B.mk_button(label, on_click, style_override)
	local S = setmetatable(style_override or {}, { __index = STYLE })

	local lbl = wibox.widget({
		text = label or "Button",
		font = S.font,
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local padded = wibox.widget({
		{ lbl, left = S.pad_h, right = S.pad_h, widget = wibox.container.margin },
		bg = S.bg,
		fg = S.fg,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, S.radius)
		end,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})

	local fixed_h = wibox.widget({ padded, strategy = "exact", height = S.height, widget = wibox.container.constraint })
	local root = wibox.widget({ fixed_h, strategy = "min", width = S.min_width, widget = wibox.container.constraint })

	local function hover()
		padded.bg = S.hover_bg
		padded.shape_border_width = S.border_bw
		padded.shape_border_color = S.border
	end
	local function idle()
		padded.bg = S.bg
		padded.shape_border_width = 0
		padded.shape_border_color = "#00000000"
	end

	root:connect_signal("mouse::enter", hover)
	root:connect_signal("mouse::leave", idle)

	root:buttons(gears.table.join(awful.button({}, 1, function()
		if type(on_click) == "function" then
			on_click()
		end
	end)))

	function root:set_focus(on)
		if on then
			hover()
		else
			idle()
		end
	end

	function root:activate()
		if type(on_click) == "function" then
			on_click()
		end
	end

	root.mouse_enter_target = root
	return root
end

return B
