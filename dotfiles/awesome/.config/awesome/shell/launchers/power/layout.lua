-- ~/.config/awesome/shell/launchers/power/layout.lua
local wibox = require("wibox")

local L = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function fixed_cell(widget, width)
	return wibox.widget({
		{
			widget,
			halign = "center",
			valign = "center",
			widget = wibox.container.place,
		},
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

local function compute_cell_geom(th, body_h)
	local pad_h = assert(tonumber(th.pad_h), "power.layout: pad_h fehlt/ungültig")
	local pad_v = assert(tonumber(th.pad_v), "power.layout: pad_v fehlt/ungültig")
	local icon_ratio = assert(tonumber(th.icon_ratio), "power.layout: icon_ratio fehlt/ungültig")

	local icon_pad = assert(tonumber(th.icon_pad), "power.layout: icon_pad fehlt/ungültig")
	local cell_pad = assert(tonumber(th.icon_cell_pad), "power.layout: icon_cell_pad fehlt/ungültig")
	local extra_w = assert(tonumber(th.icon_cell_extra_w), "power.layout: icon_cell_extra_w fehlt/ungültig")
	local spacing = assert(tonumber(th.icon_spacing), "power.layout: icon_spacing fehlt/ungültig")

	local base_side = math.max(1, body_h + pad_v * 2)
	local icon_size0 = math.floor(base_side * icon_ratio)
	local icon_size = math.max(8, math.min(icon_size0, math.max(8, body_h - 2 * pad_v)))
	local cell_w = icon_size + icon_pad * 2 + cell_pad * 2 + extra_w

	return {
		pad_h = pad_h,
		icon_size = icon_size,
		cell_w = cell_w,
		spacing = spacing,
	}
end

local function resolve_close_fn(get_close_ref)
	if type(get_close_ref) ~= "function" then
		return function() end
	end

	local ok, close_fn = pcall(get_close_ref)
	if ok and type(close_fn) == "function" then
		return close_fn
	end

	return function() end
end

-- ============================================================================
-- Public API
-- ============================================================================

function L.build_row(actions, th, dims, deps, get_close_ref)
	actions = actions or {}
	deps = deps or {}

	assert(type(deps.mk_icon_button) == "function", "power.layout: deps.mk_icon_button fehlt")
	assert(dims and tonumber(dims.body_h), "power.layout: dims.body_h fehlt/ungültig")

	-- ------------------------------------------------------------------------
	-- Config
	-- ------------------------------------------------------------------------

	local count = #actions
	local geom = compute_cell_geom(th, dims.body_h)

	-- ------------------------------------------------------------------------
	-- Cells
	-- ------------------------------------------------------------------------

	local cells = {}
	local items = {}

	for i, action in ipairs(actions) do
		local btn = deps.mk_icon_button({
			icon = action.icon,
			emoji = action.emoji,
			emoji_font = action.emoji_font,
			size = geom.icon_size,
			label = action.label,
			th = th,
			on_press = function()
				local close_fn = resolve_close_fn(get_close_ref)

				if action.on_press then
					action.on_press(close_fn)
				elseif action.autoclose then
					close_fn()
				end
			end,
		})

		items[i] = btn
		cells[i] = fixed_cell(btn, geom.cell_w)
	end

	-- ------------------------------------------------------------------------
	-- Row
	-- ------------------------------------------------------------------------

	local row = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = geom.spacing,
	})

	for i = 1, #cells do
		row:add(cells[i])
	end

	-- ------------------------------------------------------------------------
	-- Width
	-- ------------------------------------------------------------------------

	local outer_pad_h = assert(tonumber(dims.pad_h), "power.layout: dims.pad_h fehlt/ungültig")
	local spacing_total = (count > 1) and ((count - 1) * geom.spacing) or 0
	local required_w = (count > 0) and (count * geom.cell_w + spacing_total + 2 * outer_pad_h) or (2 * outer_pad_h)

	return row, items, required_w
end

return L
