-- ~/.config/awesome/shell/notify/center/controller.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function bar_cfg(cfg)
	return (cfg and cfg.bar) or {}
end

local function center_cfg(cfg)
	return ((cfg and cfg.notify) or {}).center or {}
end

local function screen_mode(cfg)
	return string.lower(tostring(bar_cfg(cfg).show_notify or "primary"))
end

local function target_screens(cfg)
	if screen_mode(cfg) == "all" then
		local out = {}

		for s in screen do
			table.insert(out, s)
		end

		return out
	end

	local primary = screen.primary or awful.screen.focused()
	return primary and { primary } or {}
end

local function key_for_screen(s)
	return tostring(tonumber(s.index) or 0)
end

local function center_list_theme(notify_theme)
	local center = (notify_theme or {}).center or {}

	return {
		entry_spacing = tonumber(center.entry_spacing) or 0,
		list_pad_top = tonumber(center.list_pad_top) or 0,
		list_pad_right = tonumber(center.list_pad_right) or 0,
		list_pad_bottom = tonumber(center.list_pad_bottom) or 0,
		list_pad_left = tonumber(center.list_pad_left) or 0,
		panel_bg = center.panel_bg or "#00000000",
	}
end

local function is_portrait_screen(s)
	if not (s and s.geometry) then
		return false
	end

	return (tonumber(s.geometry.height) or 0) > (tonumber(s.geometry.width) or 0)
end

local function visible_cards_for_screen(cfg, s)
	local center = center_cfg(cfg)

	if is_portrait_screen(s) then
		return math.max(1, tonumber(center.visible_cards_portrait) or tonumber(center.visible_cards) or 5)
	end

	return math.max(1, tonumber(center.visible_cards) or 5)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.new(args)
	args = args or {}

	local cfg = args.cfg or {}
	local ui = args.ui or {}
	local notify_theme = args.notify_theme or {}

	local Popup = args.popup
	local Layout = args.layout
	local Widget = args.widget
	local View = args.view
	local Actions = args.actions
	local History = args.history
	local List = args.list
	local State = args.state

	local popups = {}
	local controller = {}

	function controller.key_for_screen(s)
		return key_for_screen(s)
	end

	function controller.target_screens()
		return target_screens(cfg)
	end

	function controller.each_popup(fn)
		for _, popup in pairs(popups) do
			fn(popup)
		end
	end

	function controller.ensure_popup(s)
		if Popup and type(Popup.ensure) == "function" then
			return Popup.ensure(popups, key_for_screen, s)
		end

		return nil
	end

	function controller.resolve_popup_geometry(popup, height_override)
		if not (Layout and type(Layout.resolve_geometry) == "function") then
			return nil
		end

		return Layout.resolve_geometry({
			popup = popup,
			cfg = cfg,
			ui = ui,
			height_override = height_override,
		})
	end

	function controller.apply_geometry(popup)
		local geo = (popup and popup._pending_geo) or controller.resolve_popup_geometry(popup)

		if not (Popup and geo) then
			return
		end

		Popup.apply_geometry(popup, geo)

		if popup then
			popup._pending_geo = nil
		end
	end

	function controller.rebuild_popup(popup)
		local base_geo = controller.resolve_popup_geometry(popup)

		if not (Popup and Widget and List and State) then
			return nil
		end

		if not base_geo or base_geo.height <= 0 then
			local state = State.state_for_screen(popup.screen)
			State.clear_selection(state)

			return Popup.rebuild(popup, function()
				return nil
			end)
		end

		return Popup.rebuild(popup, function()
			local entries = (History and History.list and History.list()) or {}
			local state = State.state_for_screen(popup.screen)
			local list_theme = center_list_theme(notify_theme)
			local visible_limit = visible_cards_for_screen(cfg, popup.screen)

			local list_width = base_geo.width
			if Layout and type(Layout.resolve_list_width) == "function" then
				list_width = Layout.resolve_list_width(base_geo.width)
			end

			local built = List.build({
				theme = list_theme,
				entries = entries,
				cfg = cfg,
				ui = ui,
				state = state,
				max_height = base_geo.height,
				list_width = list_width,
				visible_limit = visible_limit,
				widget = Widget,
				deps = {
					actions = Actions,
					theme = notify_theme,
				},
			})

			if not built then
				State.clear_selection(state)
				return nil
			end

			state.items = built.items or {}
			state.last_total = built.total or #entries
			state.last_visible = built.visible or #(state.items or {})

			if List and type(List.clamp_scroll_offset) == "function" then
				List.clamp_scroll_offset(state)
			end

			if type(State.set_selected_index) == "function" then
				if state.selected_index ~= nil then
					State.set_selected_index(popup.screen, state.selected_index)
				else
					State.ensure_selected(popup.screen)
				end
			end

			local panel = built.widget

			if View and type(View.build) == "function" then
				panel = View.build({
					widget = built.widget,
					cfg = cfg,
					ui = ui,
					popup_width = base_geo.width,
					popup_height = base_geo.height,
					pad_top = list_theme.list_pad_top,
					pad_right = list_theme.list_pad_right,
					pad_bottom = list_theme.list_pad_bottom,
					pad_left = list_theme.list_pad_left,
					bg = list_theme.panel_bg,
				})
			end

			if not panel then
				State.clear_selection(state)
				return nil
			end

			local _, fitted_h = panel:fit({}, base_geo.width, base_geo.height)
			fitted_h = tonumber(fitted_h) or base_geo.height
			if fitted_h < 1 then
				fitted_h = base_geo.height
			end

			popup._pending_geo = controller.resolve_popup_geometry(popup, fitted_h)

			return panel
		end)
	end

	function controller.sync_popups()
		if not (Popup and type(Popup.sync) == "function") then
			return
		end

		Popup.sync({
			popups = popups,
			screens = controller.target_screens(),
			key_for_screen = key_for_screen,
			ensure_popup = controller.ensure_popup,
			rebuild_popup = controller.rebuild_popup,
			apply_geometry = controller.apply_geometry,
		})
	end

	function controller.refresh_screen(s)
		local popup = popups[key_for_screen(s)]
		if popup then
			controller.rebuild_popup(popup)
			controller.apply_geometry(popup)
		end
	end

	function controller.scroll_delta(s, delta)
		if not (State and List and type(List.scroll_delta) == "function") then
			return false
		end

		local state = State.state_for_screen(s)
		local current = tonumber(state.scroll_offset) or 0
		local next_offset = List.scroll_delta(state, delta)

		if next_offset == current then
			return false
		end

		controller.refresh_screen(s)
		return true
	end

	function controller.select_prev(s)
		if not State then
			return
		end

		s = s or screen.primary or awful.screen.focused()

		local state = State.state_for_screen(s)
		local visible = tonumber(state.last_visible) or #(state.items or {})
		local current = tonumber(state.selected_index)

		if visible < 1 then
			return
		end

		if current == nil then
			State.set_selected_index(s, visible)
			return
		end

		if current > 1 then
			State.set_selected_index(s, current - 1)
			return
		end

		if controller.scroll_up(s) then
			local new_state = State.state_for_screen(s)
			local edge = math.min(1, tonumber(new_state.last_visible) or 1)
			State.set_selected_index(s, edge)
			controller.refresh_screen(s)
		end
	end

	function controller.select_next(s)
		if not State then
			return
		end

		s = s or screen.primary or awful.screen.focused()

		local state = State.state_for_screen(s)
		local visible = tonumber(state.last_visible) or #(state.items or {})
		local current = tonumber(state.selected_index)

		if visible < 1 then
			return
		end

		if current == nil then
			State.set_selected_index(s, 1)
			return
		end

		if current < visible then
			State.set_selected_index(s, current + 1)
			return
		end

		if controller.scroll_down(s) then
			local new_state = State.state_for_screen(s)
			local edge = math.max(1, tonumber(new_state.last_visible) or visible)
			State.set_selected_index(s, edge)
			controller.refresh_screen(s)
		end
	end

	function controller.activate_selected(s)
		if not State then
			return
		end

		s = s or screen.primary or awful.screen.focused()
		State.activate_selected(s)
	end

	function controller.dismiss_selected(s)
		if not State then
			return
		end

		s = s or screen.primary or awful.screen.focused()
		State.dismiss_selected(s)

		local popup = popups[key_for_screen(s)]
		if popup then
			controller.rebuild_popup(popup)
			controller.apply_geometry(popup)
		end
	end

	function controller.clear_history()
		if Actions and type(Actions.clear_history) == "function" then
			Actions.clear_history(cfg)
		elseif History and type(History.clear) == "function" then
			History.clear()
		end

		for _, s in ipairs(controller.target_screens()) do
			local state = State and State.state_for_screen and State.state_for_screen(s) or nil
			if state and type(State.clear_selection) == "function" then
				State.clear_selection(state)
			end
		end

		awesome.emit_signal("notify::close_center")
	end

	function controller.set_visible(open)
		if not (Popup and type(Popup.set_visible) == "function") then
			return
		end

		Popup.set_visible({
			popups = popups,
			screens = controller.target_screens(),
			key_for_screen = key_for_screen,
			sync_popups = controller.sync_popups,
			close_on_click_outside = (center_cfg(cfg).close_on_click_outside == true),
			on_close_request = function()
				awesome.emit_signal("notify::close_center")
			end,
			on_open = function()
				if not State then
					return
				end

				for _, s in ipairs(controller.target_screens()) do
					if type(State.reset_view) == "function" then
						State.reset_view(s, List)
					end

					local popup = popups[key_for_screen(s)]
					if popup then
						controller.rebuild_popup(popup)
						controller.apply_geometry(popup)
					end
				end
			end,
			on_close = function()
				if not State then
					return
				end

				for _, s in ipairs(controller.target_screens()) do
					State.clear_selection(State.state_for_screen(s))
				end
			end,
		}, open)
	end

	function controller.scroll_up(s)
		s = s or screen.primary or awful.screen.focused()
		return controller.scroll_delta(s, 1)
	end

	function controller.scroll_down(s)
		s = s or screen.primary or awful.screen.focused()
		return controller.scroll_delta(s, -1)
	end

	return controller
end

return M
