-- features/shell/menu/dialogs/parts/helpers.lua
-- Generische Layout-/Geometrie-Utilities (keine Popup-/Theme-Resolve-Logik)

local wibox = require("wibox")
local gears = require("gears")

local H = {}

function H.pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- --- Mini-Layout-Bausteine -------------------------------------------------

function H.spacer_exact(px)
	return wibox.widget({
		wibox.widget({}),
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end

function H.fixed_cell(widget, width)
	return wibox.widget({
		{ widget, halign = "center", valign = "center", widget = wibox.container.place },
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

function H.targets_linear(n)
	local t = {}
	if n and n > 0 then
		for i = 1, n do
			t[i] = (i - 0.5) / n
		end
	end
	return t
end

-- verteilt n Zellen (Breite icon_px_w) entlang Targets (0..1) auf place_px_w
function H.build_even_row(icon_cells, targets, icon_px_w, place_px_w)
	local n = #icon_cells
	local S = {}
	if n == 1 then
		local x = targets[1] * place_px_w
		S[1] = math.max(0, x - icon_px_w / 2)
		S[2] = math.max(0, place_px_w - (S[1] + icon_px_w))
	else
		local sum = 0
		S[1] = math.max(0, targets[1] * place_px_w - icon_px_w / 2)
		sum = sum + S[1]
		for i = 2, n do
			local left_needed = targets[i] * place_px_w - icon_px_w / 2
			local si = left_needed - (sum + (i - 1) * icon_px_w)
			S[i] = math.max(0, si)
			sum = sum + S[i]
		end
		S[n + 1] = math.max(0, place_px_w - (sum + n * icon_px_w))
	end

	local row = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	for i = 1, n do
		row:add(H.spacer_exact(S[i]))
		row:add(icon_cells[i])
	end
	row:add(H.spacer_exact(S[n + 1] or 0))
	return row
end

-- --- Geometrie -------------------------------------------------------------

function H.compute_dialog_segments(th, Wd, Hd)
	local pick = H.pick
	th = th or {}
	local HR = pick(th.header_ratio, 0.18)
	local FR = pick(th.footer_ratio, 0.18)
	local h_header = math.floor(Hd * HR)
	local h_footer = math.floor(Hd * FR)
	local h_body = Hd - h_header - h_footer
	return { header = h_header, body = h_body, footer = h_footer }
end

function H.compute_icon_metrics(th, Wd, Hd)
	local pick = H.pick
	th = th or {}
	local seg = H.compute_dialog_segments(th, Wd, Hd)

	local base_side = math.min(Wd, Hd)
	local ICON_SIZE_RAW = math.floor(base_side * pick(th.icon_ratio, 0.20))
	local PAD_V = pick(th.pad_v, 14)
	local ICON_MAX = math.max(8, seg.body - 2 * PAD_V)
	local ICON_SIZE = math.min(ICON_SIZE_RAW, ICON_MAX)

	local icon_pad = pick(th.icon_pad, 6)
	local cell_pad = pick(th.icon_cell_pad, 6)
	local cell_extra = pick(th.icon_cell_extra_w, 12)
	local ICON_CELL_W = ICON_SIZE + icon_pad * 2 + cell_pad * 2 + cell_extra

	local PAD_H = pick(th.pad_h, 16)
	local place_w = Wd - 2 * PAD_H

	return {
		icon_size = ICON_SIZE,
		icon_cell_w = ICON_CELL_W,
		pad_h = PAD_H,
		pad_v = PAD_V,
		place_w = place_w,
		seg = seg,
	}
end

return H
