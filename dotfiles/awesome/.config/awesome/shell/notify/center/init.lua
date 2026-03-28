local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	controller = nil,

	popup = nil,
	layout = nil,
	signals = nil,
	widget = nil,
	view = nil,
	actions = nil,
	list = nil,
	state = nil,
	controller_mod = nil,
}

local runtime = {
	ctx = {},
	signals_ready = false,
	history = nil,
	notify_theme = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function controller()
	return M.controller
end

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function notify_theme()
	return runtime.notify_theme or {}
end

local function is_signals_ready()
	return runtime.signals_ready == true
end

local function set_signals_ready(value)
	runtime.signals_ready = (value == true)
end

local function current_screen(s)
	return s or screen.primary or awful.screen.focused()
end

local function register_signals()
	local Signals = M.signals
	local State = M.state
	local List = M.list
	local Controller = M.controller

	if not (Signals and type(Signals.register) == "function" and Controller) then
		return
	end

	Signals.register({
		cfg = cfg(),
		ui = ui(),
		is_ready = is_signals_ready,
		set_ready = set_signals_ready,
		set_visible = Controller.set_visible,
		each_popup = Controller.each_popup,
		rebuild_popup = Controller.rebuild_popup,
		apply_geometry = Controller.apply_geometry,
		sync_popups = Controller.sync_popups,

		scroll_up = function()
			Controller.scroll_up(current_screen())
		end,
		scroll_down = function()
			Controller.scroll_down(current_screen())
		end,

		select_prev = function()
			M.select_prev(current_screen())
		end,

		select_next = function()
			M.select_next(current_screen())
		end,

		activate_selected = function()
			if type(Controller.activate_selected) == "function" then
				Controller.activate_selected(current_screen())
			end
		end,
		dismiss_selected = function()
			if type(Controller.dismiss_selected) == "function" then
				Controller.dismiss_selected(current_screen())
			end
		end,
		clear_history = function()
			if type(Controller.clear_history) == "function" then
				Controller.clear_history()
			end
		end,

		before_history_rebuild = function(popup)
			if State and type(State.prepare_history_update) == "function" then
				State.prepare_history_update(popup, runtime.history)
			end
		end,
		after_history_rebuild = function(popup)
			if State and type(State.finalize_history_update) == "function" then
				State.finalize_history_update(popup, List)
			end
		end,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	runtime.ctx = args
	runtime.history = args.history or runtime.history
	runtime.notify_theme = args.notify_theme or {}

	M.popup = safe_require("shell.notify.center.popup")
	M.layout = safe_require("shell.notify.center.layout")
	M.signals = safe_require("shell.notify.center.signals")
	M.widget = safe_require("shell.notify.center.widget")
	M.view = safe_require("shell.notify.center.view")
	M.actions = safe_require("shell.notify.center.actions")
	M.list = safe_require("shell.notify.center.list")
	M.state = safe_require("shell.notify.center.state")
	M.controller_mod = safe_require("shell.notify.center.controller")

	if M.actions and type(M.actions.init) == "function" then
		M.actions.init({
			history = runtime.history,
		})
	end

	if M.controller_mod and type(M.controller_mod.new) == "function" then
		M.controller = M.controller_mod.new({
			cfg = cfg(),
			ui = ui(),
			notify_theme = notify_theme(),
			popup = M.popup,
			layout = M.layout,
			widget = M.widget,
			view = M.view,
			actions = M.actions,
			history = runtime.history,
			list = M.list,
			state = M.state,
		})
	end

	if M.controller then
		M.controller.sync_popups()
	end

	register_signals()

	return M
end

function M.selected_index(s)
	local State = M.state
	if not State then
		return nil
	end

	local state = State.state_for_screen(current_screen(s))
	return state.selected_index
end

function M.select_next(s)
	local State = M.state
	if not State then
		return
	end

	s = current_screen(s)
	local state = State.state_for_screen(s)
	local current = state.selected_index or 0
	State.set_selected_index(s, current + 1)
end

function M.select_prev(s)
	local State = M.state
	if not State then
		return
	end

	s = current_screen(s)
	local state = State.state_for_screen(s)
	local current = state.selected_index

	if current == nil then
		current = #(state.items or {}) + 1
	end

	State.set_selected_index(s, current - 1)
end

function M.activate_selected(s)
	local Controller = controller()
	if Controller and type(Controller.activate_selected) == "function" then
		Controller.activate_selected(current_screen(s))
	end
end

function M.dismiss_selected(s)
	local Controller = controller()
	if Controller and type(Controller.dismiss_selected) == "function" then
		Controller.dismiss_selected(current_screen(s))
	end
end

function M.clear_history()
	local Controller = controller()
	if Controller and type(Controller.clear_history) == "function" then
		Controller.clear_history()
	end
end

function M.scroll_up(s)
	local Controller = controller()
	if Controller and type(Controller.scroll_up) == "function" then
		Controller.scroll_up(current_screen(s))
	end
end

function M.scroll_down(s)
	local Controller = controller()
	if Controller and type(Controller.scroll_down) == "function" then
		Controller.scroll_down(current_screen(s))
	end
end

return M
