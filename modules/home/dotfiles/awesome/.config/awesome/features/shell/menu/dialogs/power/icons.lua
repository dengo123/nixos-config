-- ~/.config/awesome/features/shell/menu/dialogs/parts/icons.lua
local wibox = require("wibox")
local W = require("features.shell.menu.dialogs.parts.widgets")
local Lib = require("features.shell.menu.lib")

local H = Lib.helpers or {}

local Icons = {}

function Icons.compute_metrics(th, dialog_w, dialog_h)
	-- falls du die zentrale Variante verwendest:
	if H.compute_icon_metrics then
		return H.compute_icon_metrics(th, dialog_w, dialog_h)
	end
	-- ansonsten hier die bisherige compute_metrics lassen (oder löschen, wenn zentral genutzt)
	-- ... (du kannst auch einfach 'return H.compute_icon_metrics(...)' machen und den Rest streichen)
end

function Icons.actions_row(actions, th, geom, get_close_ref)
	actions = actions or {}
	geom = geom or {}

	local cells, items = {}, {}

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
				else
					-- datengetriebener Fallback via Lib.actions.run (falls genutzt)
					local Actions = Lib.actions
					if Actions and Actions.run then
						if a.autoclose and close_fn then
							close_fn()
						end
						Actions.run(a)
					end
				end
			end,
		})

		items[i] = btn
		local cell_w = geom.icon_cell_w or (geom.icon_size or 64) + 24
		cells[i] = (H.fixed_cell and H.fixed_cell(btn, cell_w)) or btn
	end

	if #cells == 0 then
		return wibox.widget({ layout = wibox.layout.fixed.horizontal }), {}
	end

	local targets = (H.targets_linear and H.targets_linear(#cells)) or {}
	local row = (H.build_even_row and H.build_even_row(cells, targets, geom.icon_cell_w, geom.place_w or 400))
		or wibox.widget({ layout = wibox.layout.fixed.horizontal })

	return row, items
end

return Icons
