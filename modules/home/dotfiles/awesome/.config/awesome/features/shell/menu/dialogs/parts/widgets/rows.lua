local awful, gears, wibox = require("awful"), require("gears"), require("wibox")

local R = {}
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- XP-like Kategorie: breite Kachel mit Icon links und Titel; Fokus/Hover auf der Kachel
function R.mk_category_button(args)
	args = args or {}
	local th = args.th or {}
	local size = args.size or pick(th.cat_icon_size, 36)
	local pad = pick(th.cat_pad, 10)

	-- Icon (emoji oder image)
	local icon
	if args.icon then
		icon = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	else
		local emoji = args.emoji or "…"
		icon = wibox.widget({
			markup = ("<span font='sans %d'>%s</span>"):format(math.floor(size * 0.9), emoji),
			align = "center",
			valign = "center",
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.textbox,
		})
	end

	local title = wibox.widget({
		markup = ("<span font='sans %d'><b>%s</b></span>"):format(pick(th.cat_font_size, 16), args.label or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local content = wibox.widget({
		{ icon, halign = "center", valign = "center", widget = wibox.container.place },
		{ title, halign = "left", valign = "center", widget = wibox.container.place },
		spacing = pick(th.cat_spacing, 10),
		layout = wibox.layout.fixed.horizontal,
	})

	local bg = wibox.widget({
		{ content, left = pad, right = pad, top = pad, bottom = pad, widget = wibox.container.margin },
		bg = th.cat_bg or "#00000015",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, pick(th.cat_radius, 8))
		end,
		widget = wibox.container.background,
	})

	-- Hover / Fokus
	local function set_visual(on, t)
		t = t or th
		bg.bg = on and (t.cat_hover_bg or "#FFFFFF22") or (t.cat_bg or "#00000015")
		bg.shape_border_width = on and (t.cat_hover_bw or 2) or 0
		bg.shape_border_color = t.cat_hover_border or "#2B77FF"
	end
	bg:connect_signal("mouse::enter", function()
		set_visual(true)
	end)
	bg:connect_signal("mouse::leave", function()
		set_visual(false)
	end)

	if args.on_press then
		bg:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	-- Fokus-API wie bei Icon-Button
	function bg:set_focus(on, th2)
		set_visual(on, th2)
	end

	function bg:activate()
		if args.on_press then
			args.on_press()
		end
	end

	return bg
end

return R
