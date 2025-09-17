local wibox = require("wibox")
local theme = require("features.shell.menu.widgets.theme")

local M = {}

function M.apply_hover(bg_container, t, normal, hover)
	t = theme.with_defaults(t)
	local normal_bg = normal or t.bg
	local hover_bg = hover or t.bg_focus
	if normal_bg:lower() == hover_bg:lower() then
		hover_bg = theme.adjust(normal_bg, -12)
	end
	bg_container:connect_signal("mouse::enter", function()
		bg_container.bg = hover_bg
	end)
	bg_container:connect_signal("mouse::leave", function()
		bg_container.bg = normal_bg
	end)
end

function M.fixed_height(widget, h)
	return wibox.widget({ widget, strategy = "exact", height = h, widget = wibox.container.constraint })
end

return M
