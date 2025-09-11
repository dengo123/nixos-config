-- features/shell/menu/footer.lua
local wibox = require("wibox")
local P = require("features.shell.menu.primitives")

local Footer = {}

-- power_items: { {icon=..., text=..., on_press=function() ... end}, ... }
function Footer.build(power_items, t)
	local powers = { layout = wibox.layout.fixed.horizontal, spacing = t.power_group_spacing or 8 }
	for _, p in ipairs(power_items or {}) do
		table.insert(powers, P.power_button(p, t))
	end

	local row = wibox.widget({
		nil,
		powers,
		nil,
		expand = "outside",
		layout = wibox.layout.align.horizontal,
	})

	return wibox.widget({
		{
			row,
			left = t.footer_pad_l or 10,
			right = t.footer_pad_r or 10,
			top = t.footer_pad_t or 6,
			bottom = t.footer_pad_b or 6,
			widget = wibox.container.margin,
		},
		bg = t.footer_bg or t.bg,
		fg = t.footer_fg or t.fg,
		widget = wibox.container.background,
	})
end

return Footer
