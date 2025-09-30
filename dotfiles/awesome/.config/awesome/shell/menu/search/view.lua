-- shell/menu/search/view.lua
-- UI-only: baut den wibox-Baum und liefert Widget + wichtige Handles zurück.
-- Erwartet, dass der Aufrufer die "textbox" (aus awful.widget.prompt().widget) liefert.

local wibox = require("wibox")

local V = {}

--- build(ui, textbox)
-- ui = {
--   height, width_expanded,
--   bg_active, fg_active,
--   padding = { left, right, top, bottom },
--   spacing
-- }
-- textbox: wibox.widget.textbox (z.B. aus awful.widget.prompt().widget)
function V.build(ui, textbox)
	assert(textbox, "search.view: textbox is required")

	-- fixes Präfix-Label (nicht editierbar)
	local prefix_lbl = wibox.widget({
		text = "",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- Innenlayout
	local inner_margin = wibox.widget({
		{
			prefix_lbl,
			textbox,
			spacing = ui.spacing,
			layout = wibox.layout.fixed.horizontal,
		},
		left = ui.padding.left,
		right = ui.padding.right,
		top = ui.padding.top,
		bottom = ui.padding.bottom,
		widget = wibox.container.margin,
	})

	local bg_box = wibox.widget({
		inner_margin,
		bg = ui.bg_active,
		fg = ui.fg_active,
		widget = wibox.container.background,
	})

	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		height = ui.height,
		widget = wibox.container.constraint,
	})

	local vcenter = wibox.widget({
		height_ctl,
		valign = "center",
		widget = wibox.container.place,
	})

	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		width = ui.width_expanded,
		widget = wibox.container.constraint,
	})

	local hleft = wibox.widget({
		width_ctl,
		halign = "left",
		widget = wibox.container.place,
	})

	local widget = wibox.widget({
		hleft,
		layout = wibox.layout.fixed.horizontal,
	})

	return {
		widget = widget,
		parts = {
			textbox = textbox,
			prefix_lbl = prefix_lbl,
			inner_margin = inner_margin,
			bg_box = bg_box,
			width_ctl = width_ctl,
		},
	}
end

return V
