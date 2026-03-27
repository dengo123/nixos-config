-- ~/.config/awesome/shell/launchers/session/layout.lua
local wibox = require("wibox")

local L = {}

local runtime = {
	ctx = {},
}

-- ============================================================================
-- Helpers
-- ============================================================================

local function ctx()
	return runtime.ctx or {}
end

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
	local pad_h = assert(tonumber(th.pad_h), "session.layout: pad_h fehlt/ungültig")
	local pad_v = assert(tonumber(th.pad_v), "session.layout: pad_v fehlt/ungültig")
	local icon_ratio = assert(tonumber(th.icon_ratio), "session.layout: icon_ratio fehlt/ungültig")

	local icon_pad = assert(tonumber(th.icon_pad), "session.layout: icon_pad fehlt/ungültig")
	local cell_pad = assert(tonumber(th.icon_cell_pad), "session.layout: icon_cell_pad fehlt/ungültig")
	local extra_w = assert(tonumber(th.icon_cell_extra_w), "session.layout: icon_cell_extra_w fehlt/ungültig")
	local spacing = assert(tonumber(th.icon_spacing), "session.layout: icon_spacing fehlt/ungültig")

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

local function required_row_width(cell_count, cell_w, spacing)
	local count = math.max(0, tonumber(cell_count) or 0)
	local spacing_total = (count > 1) and ((count - 1) * spacing) or 0
	return (count * cell_w) + spacing_total
end

-- ============================================================================
-- Public API
-- ============================================================================

function L.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return L
end

function L.build_row(actions, th, dims, deps, get_close_ref)
	actions = actions or {}
	deps = deps or {}

	assert(type(deps.mk_icon_button) == "function", "session.layout: deps.mk_icon_button fehlt")
	assert(dims and tonumber(dims.body_h), "session.layout: dims.body_h fehlt/ungültig")

	-- ------------------------------------------------------------------------
	-- Config
	-- ------------------------------------------------------------------------

	local count = #actions
	local geom = compute_cell_geom(th, dims.body_h)

	local label_size = tonumber(th.icon_label_size) or 0
	local label_leading = tonumber(th.icon_label_leading) or 1
	local label_pad_top = tonumber(th.icon_label_pad_top) or 0
	local label_pad_bottom = tonumber(th.icon_label_pad_bottom) or 0
	local label_h = math.ceil(label_size * label_leading) + label_pad_top + label_pad_bottom
	local visual_offset_y = math.floor((label_h + geom.spacing) / 2) + 16

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

	local slot_count = math.max(3, count)
	local row_w = required_row_width(slot_count, geom.cell_w, geom.spacing)

	local row_spacing = geom.spacing
	if count == 2 then
		row_spacing = geom.spacing * 7
	end

	local inner_row = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = row_spacing,
	})

	if count == 2 then
		inner_row:add(cells[1])
		inner_row:add(fixed_cell(wibox.widget({}), geom.cell_w))
		inner_row:add(cells[2])
	else
		for i = 1, #cells do
			inner_row:add(cells[i])
		end

		for _ = (#cells + 1), slot_count do
			inner_row:add(fixed_cell(wibox.widget({}), geom.cell_w))
		end
	end

	local inner_h = math.max(1, dims.body_h - 2 * dims.pad_v)

	local row = wibox.widget({
		{
			{
				inner_row,
				halign = "center",
				valign = "center",
				widget = wibox.container.place,
			},
			top = visual_offset_y,
			widget = wibox.container.margin,
		},
		strategy = "exact",
		width = row_w,
		height = inner_h,
		widget = wibox.container.constraint,
	})

	-- ------------------------------------------------------------------------
	-- Width
	-- ------------------------------------------------------------------------

	local outer_pad_h = assert(tonumber(dims.pad_h), "session.layout: dims.pad_h fehlt/ungültig")
	local required_w = row_w + 2 * outer_pad_h

	return row, items, required_w
end

return L
