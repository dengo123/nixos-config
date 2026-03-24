-- ~/.config/awesome/shell/notify/center/list.lua
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function prepare_item(item, width)
	if not item then
		return nil
	end

	item._base_widget = item._base_widget or item.widget
	item.widget = wibox.widget({
		item._base_widget,
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})

	return item
end

local function measure_card_height(item, width)
	if not item then
		return 0
	end

	prepare_item(item, width)

	if not (item.widget and item.widget.fit) then
		return 0
	end

	local _, h = item.widget:fit({}, width, math.huge)
	return tonumber(h) or 0
end

local function available_height(theme, max_height)
	return math.max(0, (tonumber(max_height) or 0) - (theme.list_pad_top or 0) - (theme.list_pad_bottom or 0))
end

local function build_all_cards(theme, entries, cfg, build_card)
	-- History kommt newest-first, View braucht oldest->newest
	local out = {}
	for i = #entries, 1, -1 do
		table.insert(out, build_card(theme, entries[i], cfg))
	end
	return out
end

local function measure_all_heights(items, width)
	local heights = {}
	for i, item in ipairs(items) do
		heights[i] = measure_card_height(item, width)
	end
	return heights
end

local function max_scroll_offset_for(total, visible)
	return math.max(0, total - visible)
end

local function resolve_window(heights, spacing, avail_h, scroll_offset)
	local total = #heights
	if total == 0 then
		return nil, nil, 0
	end

	-- 0 = ganz unten, neueste Nachricht sichtbar
	local bottom_index = total - (scroll_offset or 0)
	if bottom_index < 1 then
		bottom_index = 1
	end
	if bottom_index > total then
		bottom_index = total
	end

	local top_index = bottom_index
	local used = 0
	local visible = 0

	while top_index >= 1 do
		local h = heights[top_index] or 0
		local extra = (top_index < bottom_index) and spacing or 0
		local needed = used + extra + h

		if visible > 0 and needed > avail_h then
			break
		end

		if visible == 0 and h > avail_h then
			visible = 1
			used = h
			break
		end

		used = needed
		visible = visible + 1
		top_index = top_index - 1
	end

	top_index = math.max(1, top_index + 1)
	return top_index, bottom_index, visible
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.clamp_scroll_offset(state)
	local current = tonumber(state.scroll_offset) or 0
	local total = tonumber(state.last_total) or 0
	local visible = tonumber(state.last_visible) or 0
	local max_offset = max_scroll_offset_for(total, visible)

	if current < 0 then
		current = 0
	end

	if current > max_offset then
		current = max_offset
	end

	state.scroll_offset = current
	return current
end

function M.scroll_delta(state, delta)
	state.scroll_offset = (tonumber(state.scroll_offset) or 0) + (tonumber(delta) or 0)

	if state.scroll_offset < 0 then
		state.scroll_offset = 0
	end

	return state.scroll_offset
end

function M.build(args)
	local theme = args.theme or {}
	local entries = args.entries or {}
	local cfg = args.cfg or {}
	local state = args.state or {}
	local max_height = args.max_height
	local list_width = math.max(1, tonumber(args.list_width) or 1)
	local build_card = args.build_card

	if #entries == 0 or type(build_card) ~= "function" then
		return nil
	end

	state.scroll_offset = tonumber(state.scroll_offset) or 0

	local items_all = build_all_cards(theme, entries, cfg, build_card)
	local total = #items_all
	local avail_h = available_height(theme, max_height)

	if avail_h <= 0 then
		return {
			widget = nil,
			items = {},
			total = total,
			visible = 0,
		}
	end

	local spacing = theme.entry_spacing or 0
	local heights = measure_all_heights(items_all, list_width)

	local top_index, bottom_index, visible = resolve_window(heights, spacing, avail_h, state.scroll_offset)

	if not top_index or not bottom_index then
		return {
			widget = nil,
			items = {},
			total = total,
			visible = 0,
		}
	end

	local max_offset = max_scroll_offset_for(total, visible)
	if state.scroll_offset > max_offset then
		state.scroll_offset = max_offset
		top_index, bottom_index, visible = resolve_window(heights, spacing, avail_h, state.scroll_offset)
	end

	local list = wibox.layout.fixed.vertical()
	list.spacing = spacing

	local items = {}
	for i = top_index, bottom_index do
		local item = items_all[i]
		prepare_item(item, list_width)
		table.insert(items, item)
		list:add(item.widget)
	end

	return {
		widget = list,
		items = items,
		total = total,
		visible = visible,
		top_index = top_index,
		bottom_index = bottom_index,
	}
end

return M
