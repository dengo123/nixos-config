-- features/shell/menu/dialogs/parts/layouts/icons.lua
local wibox = require("wibox")
local U = require("features.shell.menu.dialogs.parts.layouts.util")
local W = require("features.shell.menu.dialogs.parts.widgets")

local M = {}

function M.actions_row(actions, th, geom, get_close_ref)
	actions, geom = actions or {}, geom or {}
	local cells, items = {}, {}
	local cell_w = geom.icon_cell_w or ((geom.icon_size or 64) + 24)
	for i, a in ipairs(actions) do
		local btn = W.mk_icon_button({
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

function M.actions_grid(actions, th, opts, get_close_ref)
	actions, th, opts = actions or {}, th or {}, opts or {}
	local cols = tonumber(opts.cols) or 4
	local spacing = tonumber(opts.spacing) or (th.icon_grid_spacing or 10)
	local g = U.compute_metrics(th, opts.dialog_w or th.dialog_w, opts.dialog_h or th.dialog_h)
	local icon_size = tonumber(opts.size) or g.icon_size or 64
	local extra_w = tonumber(opts.cell_extra_w or th.icon_cell_extra_w) or 12
	local cell_width = icon_size + (th.icon_pad or 6) * 2 + (th.icon_cell_pad or 6) * 2 + extra_w

	local grid =
		wibox.widget({ homogeneous = true, spacing = spacing, forced_num_cols = cols, layout = wibox.layout.grid })
	local items = {}
	for i, a in ipairs(actions) do
		local btn = W.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = icon_size,
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
		grid:add(U.fixed_cell(btn, cell_width))
		items[i] = btn
	end
	return grid, items
end

return M
