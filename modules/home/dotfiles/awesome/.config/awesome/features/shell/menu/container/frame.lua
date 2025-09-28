-- ~/.config/awesome/features/shell/menu/container/frame.lua
local wibox = require("wibox")
local gears = require("gears")

local Frame = {}

-- Kompakter Helper für feste Höhe
function Frame.fixed_height(widget, h)
	return wibox.widget({
		widget,
		strategy = "exact",
		height = math.max(1, tonumber(h) or 1),
		widget = wibox.container.constraint,
	})
end

-- args = { header, body, footer }
-- t    = Theme-Table:
--       popup_radius, popup_border_width, popup_border_color,
--       dialog_bg (innen), outer_bg (außen, optional), total_height
-- opts = { total_height?, radius?, border_width?, inner_bg?, outer_bg?, border_color? }
function Frame.wrap(args, t, opts)
	t, opts = (t or {}), (opts or {})

	local R = tonumber(opts.radius or t.popup_radius) or 12
	local BW = tonumber(opts.border_width or t.popup_border_width) or 1
	local BORDC = opts.border_color or t.popup_border_color or t.header_bg or "#3A6EA5"
	local INNER = opts.inner_bg or t.dialog_bg or "#235CDB"
	local OUTER = opts.outer_bg or t.popup_bg or INNER
	local TOTAL = tonumber(opts.total_height or t.total_height)

	local inner_stack = wibox.widget({
		args.header,
		args.body,
		args.footer,
		layout = wibox.layout.fixed.vertical,
	})

	-- Innerer, abgerundeter Korpus (BG = Inhalt)
	local inner_bg = wibox.widget({
		inner_stack,
		bg = INNER,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, math.max(0, R - 0))
		end,
		shape_clip = true,
		widget = wibox.container.background,
	})

	-- Border simulieren (Außenform) via Margin + Außenfläche
	local inner_with_margin = wibox.widget({
		inner_bg,
		left = BW,
		right = BW,
		top = BW,
		bottom = BW,
		widget = wibox.container.margin,
	})

	local outer = wibox.widget({
		inner_with_margin,
		forced_height = TOTAL,
		bg = BORDC, -- Borderfarbe füllt die Außenfläche
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, R)
		end,
		shape_clip = true,
		widget = wibox.container.background,
	})

	-- Optional noch eine „Outer-BG“-Schicht (z. B. Schatten- oder Panel-Farbe)
	if OUTER and OUTER ~= BORDC then
		outer = wibox.widget({
			{
				inner_with_margin,
				bg = BORDC,
				shape = function(cr, w, h)
					gears.shape.rounded_rect(cr, w, h, R)
				end,
				shape_clip = true,
				widget = wibox.container.background,
			},
			bg = OUTER,
			widget = wibox.container.background,
		})
	end

	return outer
end

return Frame
