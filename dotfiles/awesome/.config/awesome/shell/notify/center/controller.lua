-- ~/.config/awesome/shell/notify/center/controller.lua
local awful = require("awful")
local beautiful = require("beautiful")

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

local function resolve_popup_geometry(Popup, popup, cfg, height_override)
	if not (popup and popup.screen and Popup) then
		return nil
	end

	local theme = Popup.build_theme and Popup.build_theme() or {}
	local width = Popup.resolve_width and Popup.resolve_width(theme, popup.screen) or 0
	local max_height = Popup.resolve_max_height and Popup.resolve_max_height(theme, popup.screen) or 0
	local bar_position = tostring(bar_cfg(cfg).position or "bottom")

	local height = tonumber(height_override) or max_height
	if max_height > 0 and height > max_height then
		height = max_height
	end

	if height <= 0 then
		return {
			x = 0,
			y = 0,
			width = width,
			height = 0,
		}
	end

	if Popup.resolve_position then
		return Popup.resolve_position(theme, popup.screen, bar_position, width, height)
	end

	return nil
end

local function center_theme()
	local notify = beautiful.notify or {}
	return notify.center or {}
end

local function center_list_theme()
	local center = center_theme()

	return {
		entry_spacing = tonumber(center.entry_spacing) or 0,
		list_pad_top = tonumber(center.list_pad_top) or 0,
		list_pad_right = tonumber(center.list_pad_right) or 0,
		list_pad_bottom = tonumber(center.list_pad_bottom) or 0,
		list_pad_left = tonumber(center.list_pad_left) or 0,
		panel_bg = center.panel_bg or "#00000000",
	}
end

local function list_width_for_popup(width)
	local t = center_theme()
	local pad_left = tonumber(t.list_pad_left) or 0
	local pad_right = tonumber(t.list_pad_right) or 0
	return math.max(1, (tonumber(width) or 1) - pad_left - pad_right)
end

local function visible_cards_for_screen(cfg, s)
	local center = center_cfg(cfg)
	local fallback = tonumber(center.visible_cards) or 5

	if not s or not s.geometry then
		return fallback
	end

	local g = s.geometry
	local is_portrait = tonumber(g.height) > tonumber(g.width)

	if is_portrait then
		return tonumber(center.visible_cards_portrait) or fallback
	end

	return tonumber(center.visible_cards_landscape) or fallback
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.new(args)
	local cfg = args.cfg or {}
	local Popup = args.popup
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
		return resolve_popup_geometry(Popup, popup, cfg, height_override)
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
			local theme = center_list_theme()
			local list_width = list_width_for_popup(base_geo.width)
			local visible_limit = visible_cards_for_screen(cfg, popup.screen)

			local built = List.build({
				theme = theme,
				entries = entries,
				cfg = cfg,
				state = state,
				max_height = base_geo.height,
				list_width = list_width,
				visible_limit = visible_limit,
				widget = Widget,
				deps = {
					actions = Actions,
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

			State.ensure_selected(popup.screen)

			local panel = built.widget

			if View and type(View.build) == "function" then
				panel = View.build({
					widget = built.widget,
					popup_width = base_geo.width,
					popup_height = base_geo.height,
					pad_top = theme.list_pad_top,
					pad_right = theme.list_pad_right,
					pad_bottom = theme.list_pad_bottom,
					pad_left = theme.list_pad_left,
					bg = theme.panel_bg,
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

			popup._pending_geo = resolve_popup_geometry(Popup, popup, cfg, fitted_h)

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

	function controller.scroll_delta(s, delta)
		if not (State and List and type(List.scroll_delta) == "function") then
			return
		end

		local state = State.state_for_screen(s)
		local current = tonumber(state.scroll_offset) or 0
		local next_offset = List.scroll_delta(state, delta)

		if next_offset == current then
			return
		end

		local popup = popups[key_for_screen(s)]
		if popup then
			controller.rebuild_popup(popup)
			controller.apply_geometry(popup)
		end
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

					State.ensure_selected(s)
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
		controller.scroll_delta(s, 1)
	end

	function controller.scroll_down(s)
		s = s or screen.primary or awful.screen.focused()
		controller.scroll_delta(s, -1)
	end

	return controller
end

return M
