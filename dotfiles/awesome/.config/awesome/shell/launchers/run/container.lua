-- ~/.config/awesome/shell/launchers/run/container.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- build(th, dims, { title, body, ok_btn, cancel_btn, footer })
-- footer optional, ok/cancel bevorzugt
function M.build(th, dims, w)
	-- 🔹 Runde Ecken & Rand am OUTER-Container (nicht im Popup)
	local radius = tonumber(pick(th.panel_radius, 12))
	local border_w = tonumber(pick(th.panel_border_width, 2)) -- 2px
	local border_color = pick(th.panel_border, th.header_bg) -- Rand = Header-Farbe
	local header_h = tonumber(pick(th.header_h, 28)) -- Theme: header_h
	local footer_h = tonumber(pick(th.footer_h, dims.footer_h))

	-- ===== Header =====
	local title = wibox.widget({
		text = w.title or (th.title or "Launcher"),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local closeX = wibox.widget({
		{ text = "✖", align = "center", valign = "center", widget = wibox.widget.textbox },
		forced_width = 28,
		forced_height = 22,
		shape = gears.shape.rounded_bar,
		bg = "#00000000",
		widget = wibox.container.background,
	})
	if w.cancel_btn and w.cancel_btn.activate then
		closeX:buttons(gears.table.join(require("awful").button({}, 1, function()
			w.cancel_btn:activate()
		end)))
	end

	local header = wibox.widget({
		{
			{ title, halign = "left", widget = wibox.container.place },
			{ closeX, halign = "right", widget = wibox.container.place },
			layout = wibox.layout.align.horizontal,
		},
		left = pick(th.header_pad_h, th.pad_h, 12),
		right = pick(th.header_pad_h, th.pad_h, 8),
		widget = wibox.container.margin,
	})
	header = wibox.widget({
		header,
		bg = pick(th.panel_header_bg, pick(th.header_bg, "#235CDB")),
		fg = pick(th.panel_header_fg, pick(th.header_fg, "#FFFFFF")),
		widget = wibox.container.background,
	})

	-- ===== Body =====
	local body = wibox.widget({
		{
			w.body,
			left = pick(th.panel_pad_h, th.pad_h, 12),
			right = pick(th.panel_pad_h, th.pad_h, 12),
			top = pick(th.panel_pad_v, th.pad_v, 12),
			bottom = pick(th.panel_pad_v, th.pad_v, 12),
			widget = wibox.container.margin,
		},
		bg = pick(th.panel_body_bg, pick(th.body_bg, "#1d2f6f")),
		fg = pick(th.panel_body_fg, pick(th.body_fg, "#FFFFFF")),
		widget = wibox.container.background,
	})

	-- ===== Footer: OK + Cancel rechtsbündig, vertikal mittig =====
	local footer = nil
	do
		local ok_btn = w.ok_btn
		local cancel_btn = w.cancel_btn

		if not w.footer and (ok_btn or cancel_btn) then
			local row = wibox.widget({
				nil,
				{
					ok_btn or wibox.widget({}),
					cancel_btn or wibox.widget({}),
					spacing = pick(th.footer_spacing, 8),
					layout = wibox.layout.fixed.horizontal, -- Buttons behalten Eigenbreite
				},
				expand = "outside",
				layout = wibox.layout.align.horizontal,
			})

			footer = wibox.widget({
				{ row, halign = "right", valign = "center", widget = wibox.container.place },
				left = pick(th.footer_pad_h, th.pad_h, 12),
				right = pick(th.footer_pad_h, th.pad_h, 12),
				top = pick(th.footer_pad_v, th.pad_v, 8),
				bottom = pick(th.footer_pad_v, th.pad_v, 8),
				widget = wibox.container.margin,
			})

			footer = wibox.widget({
				footer,
				bg = pick(th.panel_footer_bg, pick(th.footer_bg, pick(th.panel_body_bg, "#1d2f6f"))),
				fg = pick(th.panel_footer_fg, pick(th.footer_fg, "#FFFFFF")),
				widget = wibox.container.background,
			})
		else
			footer = w.footer -- komplett extern geliefert
		end
	end

	-- ===== Dims =====
	dims.header_h = header_h
	dims.footer_h = footer and (footer_h or 40) or 0
	dims.body_h = math.max(0, dims.h - dims.header_h - dims.footer_h)

	-- ===== Stack (Header/Body[/Footer]) =====
	local stack = wibox.widget({
		{ header, strategy = "exact", height = dims.header_h, widget = wibox.container.constraint },
		{ body, strategy = "exact", height = dims.body_h, widget = wibox.container.constraint },
		layout = wibox.layout.fixed.vertical,
	})

	if footer then
		stack = wibox.widget({
			stack,
			{ footer, strategy = "exact", height = dims.footer_h, widget = wibox.container.constraint },
			layout = wibox.layout.fixed.vertical,
		})
	end

	-- ===== OUTER (gefüllter Ring als Rand) + INNEN rund geklippt =====
	-- Wir zeichnen den Rand nicht als Stroke, sondern als gefüllten Ring (bg=border_color),
	-- und legen die innere, rund geklippte Fläche mit border_w Abstand hinein.
	local inner_radius = math.max(0, radius - border_w)

	local inner_bg = wibox.widget({
		{
			stack,
			widget = wibox.container.background,
		},
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, inner_radius)
		end,
		shape_clip = true,
		bg = "#00000000", -- transparent: Ecken zeigen den äußeren Ring (nicht Root!)
		widget = wibox.container.background,
	})

	local inset = wibox.widget({
		inner_bg,
		left = border_w,
		right = border_w,
		top = border_w,
		bottom = border_w,
		widget = wibox.container.margin,
	})

	return wibox.widget({
		inset,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		shape_clip = true,
		bg = border_color, -- gefüllter Außenring in Header-Farbe
		widget = wibox.container.background,
	})
end

return M
