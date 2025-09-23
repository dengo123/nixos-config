-- ~/.config/awesome/features/shell/menu/dialogs/parts/icons.lua
local wibox = require("wibox")
local W = require("features.shell.menu.dialogs.parts.widgets")

local Icons = {}

-- lokale row-tools ...
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
local function targets_linear(n)
	local t = {}
	for i = 1, (n or 0) do
		t[i] = (i - 0.5) / n
	end
	return t
end
local function build_even_row(cells, targets, cell_w, place_w)
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
		row:add(spacer_exact(S[i]))
		row:add(cells[i])
	end
	row:add(spacer_exact(S[n + 1] or 0))
	return row
end

-- === public ===

-- Icon-Metrics für eine gegebene Dialoggröße + Theme
function Icons.compute_metrics(th, dialog_w, dialog_h)
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

function Icons.actions_row(actions, th, geom, get_close_ref)
	actions = actions or {}
	geom = geom or {}
	local cells = {}
	for i, a in ipairs(actions) do
		local btn = W.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = geom.icon_size or 64,
			label = a.label,
			th = th,
			on_press = function()
				local close_fn = nil
				if type(get_close_ref) == "function" then
					local ok, cf = pcall(get_close_ref)
					if ok then
						close_fn = cf
					end
				end
				if a.on_press then
					a.on_press(close_fn or function() end)
				end
			end,
		})
		cells[i] = fixed_cell(btn, geom.icon_cell_w or (geom.icon_size or 64) + 24)
	end
	if #cells == 0 then
		return wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end
	local targets = targets_linear(#cells)
	return build_even_row(cells, targets, geom.icon_cell_w, geom.place_w or 400)
end

return Icons
