-- ~/.config/awesome/shell/menu/power/layout.lua
-- Layout nur für Power: baut aus Actions eine Reihe Buttons, liefert auch benötigte Breite zurück
local wibox = require("wibox")
local Widgets = require("shell.menu.parts.widgets")

local L = {}

local function spacer_exact(px)
	return wibox.widget({
		wibox.widget({}),
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end
local function fixed_cell(widget, width)
	return wibox.widget({
		{ widget, halign = "center", valign = "center", widget = wibox.container.place },
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

local function build_even_row(cells, place_w)
	local n = #cells
	local row = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	if n == 0 then
		return row
	end
	-- kompakt: keine zusätzlichen Abstände → einfach alle Cells rein
	for i = 1, n do
		row:add(cells[i])
	end
	return row
end

-- Errechne reine Icon-/Cell-Parameter aus dem Theme
local function compute_cell_geom(th, body_h)
	local PAD_H = tonumber(th.pad_h) or 16
	local PAD_V = tonumber(th.pad_v) or 14
	local icon_ratio = tonumber(th.icon_ratio) or 0.20
	local base_side = math.max(1, body_h + PAD_V * 2) -- grobe Basis

	local icon_size0 = math.floor(base_side * icon_ratio)
	local icon_size = math.max(8, math.min(icon_size0, math.max(8, body_h - 2 * PAD_V)))
	local icon_pad = tonumber(th.icon_pad) or 6
	local cell_pad = tonumber(th.icon_cell_pad) or 6
	local extra_w = tonumber(th.icon_cell_extra_w) or 12
	local cell_w = icon_size + icon_pad * 2 + cell_pad * 2 + extra_w

	return {
		pad_h = PAD_H,
		icon_size = icon_size,
		cell_w = cell_w,
	}
end

-- actions: { { icon|emoji, label, on_press(close_fn), autoclose? }, ... }
-- Rückgabe: row_widget, focus_items, required_width
function L.build_row(actions, th, dims, get_close_ref)
	actions = actions or {}
	local n = #actions
	local g = compute_cell_geom(th, dims.body_h)
	local cells, items = {}, {}

	for i, a in ipairs(actions) do
		local btn = Widgets.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = g.icon_size,
			label = a.label,
			th = th,
			on_press = function(close_fn)
				local cf = close_fn
				if type(get_close_ref) == "function" then
					local ok, c = pcall(get_close_ref)
					if ok and c then
						cf = c
					end
				end
				if a.on_press then
					a.on_press(cf or function() end)
				elseif a.autoclose and cf then
					cf()
				end
			end,
		})
		items[i] = btn
		cells[i] = fixed_cell(btn, g.cell_w)
	end

	-- kompakte Gesamtbreite = Summe der Zellen + 2*pad_h
	local required_w = n > 0 and (n * g.cell_w + 2 * g.pad_h) or (2 * g.pad_h)
	local row = build_even_row(cells, required_w - 2 * g.pad_h)
	-- seitlicher Innenabstand
	row = wibox.widget({
		row,
		left = g.pad_h,
		right = g.pad_h,
		widget = wibox.container.margin,
	})

	return row, items, required_w
end

return L
