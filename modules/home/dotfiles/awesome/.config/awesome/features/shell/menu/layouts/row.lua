-- features/shell/menu/dialogs/parts/layouts/row.lua
local wibox = require("wibox")
local Widgets = require("features.shell.menu.widgets") -- zentraler Aggregator

local U = {}

-- utils ---------------------------------------------------------------
function U.spacer_exact(px)
	return wibox.widget({
		wibox.widget({}),
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end

function U.fixed_cell(widget, width)
	return wibox.widget({
		{ widget, halign = "center", valign = "center", widget = wibox.container.place },
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

function U.targets_linear(n)
	local t = {}
	for i = 1, (n or 0) do
		t[i] = (i - 0.5) / n
	end
	return t
end

function U.build_even_row(cells, targets, cell_w, place_w)
	local n = #cells
	if n == 0 then
		return wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end
	local S, sum = {}, 0
	if n == 1 then
		local x = targets[1] * place_w
		S[1] = math.max(0, x - cell_w / 2)
		S[2] = math.max(0, place_w - (S[1] + cell_w))
	else
		S[1] = math.max(0, targets[1] * place_w - cell_w / 2)
		sum = S[1]
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
		row:add(U.spacer_exact(S[i] or 0))
		row:add(cells[i])
	end
	row:add(U.spacer_exact(S[n + 1] or 0))
	return row
end

-- public --------------------------------------------------------------
local Layout = {}

function Layout.compute_metrics(th, dialog_w, dialog_h)
	th = th or {}
	local PAD_H = tonumber(th.pad_h) or 16
	local PAD_V = tonumber(th.pad_v) or 14
	local header_h = tonumber(th.header_h) or 56
	local footer_h = tonumber(th.footer_h) or 48
	local Wd = tonumber(dialog_w) or tonumber(th.dialog_w) or 560
	local Hd = tonumber(dialog_h) or tonumber(th.dialog_h) or 360
	local body_h = math.max(0, Hd - header_h - footer_h)
	local base_side = math.min(Wd, Hd)
	local icon_ratio = tonumber(th.icon_ratio) or 0.20
	local icon_size0 = math.floor(base_side * icon_ratio)
	local icon_size = math.min(math.max(8, body_h - 2 * PAD_V), icon_size0)
	local icon_pad = tonumber(th.icon_pad) or 6
	local cell_pad = tonumber(th.icon_cell_pad) or 6
	local extra_w = tonumber(th.icon_cell_extra_w) or 12
	local icon_cell_w = icon_size + icon_pad * 2 + cell_pad * 2 + extra_w
	local place_w = Wd - 2 * PAD_H
	return {
		icon_size = icon_size,
		icon_cell_w = icon_cell_w,
		pad_h = PAD_H,
		pad_v = PAD_V,
		place_w = place_w,
		header_h = header_h,
		footer_h = footer_h,
		body_h = body_h,
		dialog_w = Wd,
		dialog_h = Hd,
	}
end

-- HIER: wie icons.lua, nur über Widgets.mk_icon_button
function Layout.actions_row(actions, th, geom, get_close_ref)
	actions, geom = actions or {}, geom or {}
	local cells, items = {}, {}
	local cell_w = geom.icon_cell_w or ((geom.icon_size or 64) + 24)

	for i, a in ipairs(actions) do
		local btn = Widgets.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = geom.icon_size or 64,
			label = a.label,
			th = th,
			on_press = function()
				local close_fn
				if type(get_close_ref) == "function" then
					local ok, cf = pcall(get_close_ref)
					if ok then
						close_fn = cf
					end
				end
				if a.on_press then
					a.on_press(close_fn or function() end)
				elseif a.autoclose and close_fn then
					close_fn()
				end
			end,
		})
		items[i] = btn
		cells[i] = U.fixed_cell(btn, cell_w)
	end

	if #cells == 0 then
		return wibox.widget({ layout = wibox.layout.fixed.horizontal }), {}
	end
	local targets = U.targets_linear(#cells)
	local row = U.build_even_row(cells, targets, cell_w, geom.place_w or 400)
	return row, items
end

return Layout
