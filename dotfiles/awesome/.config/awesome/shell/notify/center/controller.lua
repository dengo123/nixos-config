-- ~/.config/awesome/shell/notify/center/controller.lua
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

	local primary = screen.primary or require("awful").screen.focused()
	return primary and { primary } or {}
end

local function key_for_screen(s)
	return tostring(tonumber(s.index) or 0)
end

local function resolve_popup_geometry(Popup, popup, cfg)
	if not (popup and popup.screen and Popup) then
		return nil
	end

	local theme = Popup.build_theme and Popup.build_theme() or {}
	local width = Popup.resolve_width and Popup.resolve_width(theme, popup.screen) or 0
	local max_height = Popup.resolve_max_height and Popup.resolve_max_height(theme, popup.screen) or 0
	local bar_position = tostring(bar_cfg(cfg).position or "bottom")

	if max_height <= 0 then
		return {
			x = 0,
			y = 0,
			width = width,
			height = 0,
		}
	end

	if Popup.resolve_position then
		return Popup.resolve_position(theme, popup.screen, bar_position, width, max_height)
	end

	return nil
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

	function controller.resolve_popup_geometry(popup)
		return resolve_popup_geometry(Popup, popup, cfg)
	end

	function controller.apply_geometry(popup)
		local geo = controller.resolve_popup_geometry(popup)

		if not (Popup and geo) then
			return
		end

		Popup.apply_geometry(popup, geo)
	end

	function controller.rebuild_popup(popup)
		local geo = controller.resolve_popup_geometry(popup)

		if not (Popup and Widget and State) then
			return nil
		end

		if not geo or geo.height <= 0 then
			local state = State.state_for_screen(popup.screen)
			State.clear_selection(state)

			return Popup.rebuild(popup, function()
				return nil
			end)
		end

		return Popup.rebuild(popup, function()
			local entries = (History and History.list and History.list()) or {}
			local state = State.state_for_screen(popup.screen)

			if type(Widget.build) ~= "function" then
				State.clear_selection(state)
				return nil
			end

			local built = Widget.build(entries, geo, cfg, state, {
				actions = Actions,
				list = List,
				view = View,
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
			return built.widget
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
		s = s or screen.primary or require("awful").screen.focused()
		controller.scroll_delta(s, 1)
	end

	function controller.scroll_down(s)
		s = s or screen.primary or require("awful").screen.focused()
		controller.scroll_delta(s, -1)
	end

	return controller
end

return M
