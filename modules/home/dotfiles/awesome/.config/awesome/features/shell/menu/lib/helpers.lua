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

-- genaue Breite auffüllen (Spacer)
function M.spacer_exact(px)
	return wibox.widget({
		wibox.widget({}),
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end

-- Zelle mit fixer Breite, Inhalt zentriert
function M.fixed_cell(widget, width)
	return wibox.widget({
		{ widget, halign = "center", valign = "center", widget = wibox.container.place },
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

-- gleichmäßige Zielpositionen 0..1 für n Zellen
function M.targets_linear(n)
	local t = {}
	for i = 1, (n or 0) do
		t[i] = (i - 0.5) / n
	end
	return t
end

-- Zellen über place_w verteilen (gleichmäßig), mit fester cell_w
function M.build_even_row(cells, targets, cell_w, place_w)
	local n = #cells
	if n == 0 then
		return wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end
	local S = {}
	if n == 1 then
		local x = targets[1] * place_w
		S[1] = math.max(0, x - cell_w / 2)
		S[2] = math.max(0, place_w - (S[1] + cell_w))
	else
		local sum = 0
		S[1] = math.max(0, targets[1] * place_w - cell_w / 2)
		sum = sum + S[1]
		for i = 2, n do
			local ln = targets[i] * place_w - cell_w / 2
			local si = ln - (sum + (i - 1) * cell_w)
			S[i] = math.max(0, si)
			sum = sum + S[i]
		end
		S[n + 1] = math.max(0, place_w - (sum + n * cell_w))
	end
	local row = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	for i = 1, n do
		row:add(M.spacer_exact(S[i]))
		row:add(cells[i])
	end
	row:add(M.spacer_exact(S[n + 1] or 0))
	return row
end

-- (optional) Icon-Metriken für Dialog-Layouts
function M.compute_icon_metrics(th, dialog_w, dialog_h)
	th = th or {}
	local PAD_H = tonumber(th.pad_h) or 16
	local PAD_V = tonumber(th.pad_v) or 14
	local header_h = tonumber(th.header_h) or 56
	local footer_h = tonumber(th.footer_h) or 48
	local body_h = math.max(0, (tonumber(dialog_h) or 360) - header_h - footer_h)
	local base_side = math.min(tonumber(dialog_w) or 560, tonumber(dialog_h) or 360)
	local icon_ratio = tonumber(th.icon_ratio) or 0.20
	local icon_size0 = math.floor(base_side * icon_ratio)
	local icon_size = math.min(math.max(8, body_h - 2 * PAD_V), icon_size0)
	local icon_pad = tonumber(th.icon_pad) or 6
	local cell_pad = tonumber(th.icon_cell_pad) or 6
	local cell_extra_w = tonumber(th.icon_cell_extra_w) or 12
	local icon_cell_w = icon_size + icon_pad * 2 + cell_pad * 2 + cell_extra_w
	local place_w = (tonumber(dialog_w) or 560) - 2 * PAD_H
	return {
		icon_size = icon_size,
		icon_cell_w = icon_cell_w,
		pad_h = PAD_H,
		pad_v = PAD_V,
		place_w = place_w,
		header_h = header_h,
		footer_h = footer_h,
		body_h = body_h,
	}
end

return M
