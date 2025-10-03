-- ~/.config/awesome/shell/launchers/power/layout.lua
local wibox = require("wibox")

local L = {}

local function fixed_cell(widget, width)
	return wibox.widget({
		{ widget, halign = "center", valign = "center", widget = wibox.container.place },
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

-- Errechne Icon-/Cell-Parameter aus dem Theme
local function compute_cell_geom(th, body_h)
	local PAD_H = tonumber(th.pad_h) or 16
	local PAD_V = tonumber(th.pad_v) or 14
	local icon_ratio = tonumber(th.icon_ratio) or 0.20
	local base_side = math.max(1, body_h + PAD_V * 2)

	local icon_size0 = math.floor(base_side * icon_ratio)
	local icon_size = math.max(8, math.min(icon_size0, math.max(8, body_h - 2 * PAD_V)))
	local icon_pad = tonumber(th.icon_pad) or 6
	local cell_pad = tonumber(th.icon_cell_pad) or 6
	local extra_w = tonumber(th.icon_cell_extra_w) or 12
	local cell_w = icon_size + icon_pad * 2 + cell_pad * 2 + extra_w

	local spacing = tonumber(th.icon_spacing) or 0

	return {
		pad_h = PAD_H, -- (nur informative Rückgabe; Rand macht der Container)
		icon_size = icon_size,
		cell_w = cell_w,
		spacing = spacing,
	}
end

-- actions: { { icon|emoji, label, on_press(close_fn), autoclose? }, ... }
-- deps: { mk_icon_button = function(args) -> widget end }
-- Rückgabe: row_widget, focus_items, required_width
function L.build_row(actions, th, dims, deps, get_close_ref)
	actions = actions or {}
	deps = deps or {}
	assert(type(deps.mk_icon_button) == "function", "layout: deps.mk_icon_button fehlt")

	local n = #actions
	local g = compute_cell_geom(th, dims.body_h)
	local cells, items = {}, {}

	for i, a in ipairs(actions) do
		local btn = deps.mk_icon_button({
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

	-- Zeile mit *gleichmäßigem* Zwischenraum
	local row = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = g.spacing,
	})
	for i = 1, #cells do
		row:add(cells[i])
	end

	-- Wichtig: KEIN weiteres left/right-Margin hier!
	-- Den Außenrand übernimmt dein Container über dims.pad_h.

	-- Breite: Summe Zellen + Summe Zwischenräume + linkes+rechtes Body-Padding
	local spacing_total = (n > 1) and (n - 1) * g.spacing or 0
	local required_w = (n > 0) and (n * g.cell_w + spacing_total + 2 * (tonumber(dims.pad_h) or 0))
		or (2 * (tonumber(dims.pad_h) or 0))

	return row, items, required_w
end

return L
