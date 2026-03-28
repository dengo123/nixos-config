-- ~/.config/awesome/shell/notify/center/state.lua
local M = {}

local runtime = {
	popup_state = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function key_for_screen(s)
	return tostring(tonumber(s.index) or 0)
end

local function state_store()
	return runtime.popup_state
end

local function clamp_index(state, index)
	local count = #(state.items or {})
	if count <= 0 then
		return nil
	end

	index = tonumber(index) or 1

	if index < 1 then
		return 1
	end

	if index > count then
		return count
	end

	return index
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.state_for_screen(s)
	local key = key_for_screen(s)

	state_store()[key] = state_store()[key]
		or {
			items = {},
			selected_index = nil,
			scroll_offset = 0,
			last_total = 0,
			last_visible = 0,
		}

	return state_store()[key]
end

function M.reset_view(s, List)
	local state = M.state_for_screen(s)

	M.clear_selection(state)

	state.scroll_offset = 0
	state.selected_index = nil
	state.items = {}
	state.last_total = 0
	state.last_visible = 0

	if List and type(List.clamp_scroll_offset) == "function" then
		List.clamp_scroll_offset(state)
	end
end

function M.clear_selection(state)
	if not state then
		return
	end

	for _, item in ipairs(state.items or {}) do
		if item and type(item.set_selected) == "function" then
			item.set_selected(false)
		end
	end

	state.selected_index = nil
end

function M.set_selected_index(s, index)
	local state = M.state_for_screen(s)
	local new_index = clamp_index(state, index)

	M.clear_selection(state)

	if not new_index then
		return
	end

	state.selected_index = new_index

	local item = state.items[new_index]
	if item and type(item.set_selected) == "function" then
		item.set_selected(true)
	end
end

function M.ensure_selected(s)
	local state = M.state_for_screen(s)

	if state.selected_index and state.items[state.selected_index] then
		return
	end

	if #(state.items or {}) > 0 then
		M.set_selected_index(s, 1)
	end
end

function M.activate_selected(s)
	local state = M.state_for_screen(s)
	local idx = state.selected_index
	local item = idx and state.items[idx] or nil

	if item and type(item.open) == "function" then
		item.open()
	end
end

function M.dismiss_selected(s)
	local state = M.state_for_screen(s)
	local idx = state.selected_index
	local item = idx and state.items[idx] or nil

	if item and type(item.dismiss) == "function" then
		item.dismiss()
	end
end

function M.clamp_scroll_state(s, List)
	local state = M.state_for_screen(s)

	if List and type(List.clamp_scroll_offset) == "function" then
		List.clamp_scroll_offset(state)
	end
end

function M.prepare_history_update(popup, History)
	if not (popup and popup.screen) then
		return
	end

	local state = M.state_for_screen(popup.screen)
	local old_total = tonumber(state.last_total) or 0
	local old_visible = tonumber(state.last_visible) or 0
	local old_offset = tonumber(state.scroll_offset) or 0

	local entries = (History and History.list and History.list()) or {}
	local new_total = #entries
	local delta = math.max(0, new_total - old_total)

	if old_total > 0 and old_visible > 0 and old_offset > 0 and delta > 0 then
		state.scroll_offset = old_offset + delta
	end
end

function M.finalize_history_update(popup, List)
	if not (popup and popup.screen) then
		return
	end

	M.clamp_scroll_state(popup.screen, List)
end

return M
