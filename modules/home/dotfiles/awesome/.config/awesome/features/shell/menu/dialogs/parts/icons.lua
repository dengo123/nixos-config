-- ~/.config/awesome/features/shell/menu/dialogs/parts/icons.lua
local wibox = require("wibox")
local H = require("features.shell.menu.dialogs.parts.helpers")
local W = require("features.shell.menu.dialogs.parts.widgets")

local Icons = {}

--- Baut eine horizontale Reihe aus Icon-Buttons basierend auf actions.
-- @param actions  Array von { icon=?, emoji=?, emoji_font=?, label=?, on_press=function(close) end }
-- @param th       Theme-Tabelle (Farben, Fonts etc.)
-- @param geom     Von H.compute_icon_metrics(...) (icon_size, icon_cell_w, place_w, pad_h/v, seg)
-- @param get_close_ref Optional: function() -> close_fn (wird zum Zeitpunkt des Klicks abgefragt)
function Icons.actions_row(actions, th, geom, get_close_ref)
	actions = actions or {}
	geom = geom or {}

	local cells = {}
	for i, a in ipairs(actions) do
		local btn = W.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = geom.icon_size,
			label = a.label,
			th = th,
			on_press = function()
				local close_fn = nil
				if type(get_close_ref) == "function" then
					-- zum Klickzeitpunkt aktuelle close()-Referenz holen
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
		cells[i] = H.fixed_cell(btn, geom.icon_cell_w)
	end

	if #cells == 0 then
		return wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end

	local targets = H.targets_linear(#cells)
	return H.build_even_row(cells, targets, geom.icon_cell_w, geom.place_w)
end

return Icons
