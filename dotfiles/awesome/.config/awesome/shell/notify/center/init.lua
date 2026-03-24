-- ~/.config/awesome/shell/notify/center/init.lua
local awful = require("awful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
	controller = nil,
}

local signals_ready = false
local runtime_cfg = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function notify_cfg()
	return runtime_cfg.notify or {}
end

local function center_cfg()
	return notify_cfg().center or {}
end

local function is_signals_ready()
	return signals_ready
end

local function set_signals_ready(value)
	signals_ready = (value == true)
end

local function register_escape_signal()
	if center_cfg().close_on_escape ~= true then
		return
	end
end

local function register_signals()
	local Signals = mod("signals")
	local State = mod("state")
	local List = mod("list")
	local controller = M.controller

	if not (Signals and type(Signals.register) == "function" and controller) then
		return
	end

	Signals.register({
		cfg = runtime_cfg,
		is_ready = is_signals_ready,
		set_ready = set_signals_ready,
		set_visible = controller.set_visible,
		each_popup = controller.each_popup,
		rebuild_popup = controller.rebuild_popup,
		apply_geometry = controller.apply_geometry,
		sync_popups = controller.sync_popups,
		scroll_up = function()
			controller.scroll_up(screen.primary or awful.screen.focused())
		end,
		scroll_down = function()
			controller.scroll_down(screen.primary or awful.screen.focused())
		end,
		before_history_rebuild = function(popup)
			local History = mod("history")
			if State and type(State.prepare_history_update) == "function" then
				State.prepare_history_update(popup, History)
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
	runtime_cfg = args.cfg or args or {}

	M.api = {
		ui = args.ui or {},
		popup = safe_require("shell.notify.center.popup"),
		signals = safe_require("shell.notify.center.signals"),
		widget = safe_require("shell.notify.center.widget"),
		view = safe_require("shell.notify.center.view"),
		actions = safe_require("shell.notify.center.actions"),
		history = safe_require("shell.notify.history"),
		list = safe_require("shell.notify.center.list"),
		state = safe_require("shell.notify.center.state"),
		controller = safe_require("shell.notify.center.controller"),
	}

	local ControllerMod = mod("controller")
	if ControllerMod and type(ControllerMod.new) == "function" then
		M.controller = ControllerMod.new({
			cfg = runtime_cfg,
			popup = mod("popup"),
			widget = mod("widget"),
			view = mod("view"),
			actions = mod("actions"),
			history = mod("history"),
			list = mod("list"),
			state = mod("state"),
		})
	end

	if M.controller then
		M.controller.sync_popups()
	end

	register_escape_signal()
	register_signals()

	return M
end

function M.selected_index(s)
	local State = mod("state")
	if not State then
		return nil
	end

	local state = State.state_for_screen(s or screen.primary or awful.screen.focused())
	return state.selected_index
end

function M.select_next(s)
	local State = mod("state")
	if not State then
		return
	end

	s = s or screen.primary or awful.screen.focused()
	local state = State.state_for_screen(s)
	local current = state.selected_index or 0
	State.set_selected_index(s, current + 1)
end

function M.select_prev(s)
	local State = mod("state")
	if not State then
		return
	end

	s = s or screen.primary or awful.screen.focused()
	local state = State.state_for_screen(s)
	local current = state.selected_index or 2
	State.set_selected_index(s, current - 1)
end

function M.activate_selected(s)
	local State = mod("state")
	if not (State and type(State.activate_selected) == "function") then
		return
	end

	s = s or screen.primary or awful.screen.focused()
	State.activate_selected(s)
end

function M.dismiss_selected(s)
	local State = mod("state")
	if not (State and type(State.dismiss_selected) == "function") then
		return
	end

	s = s or screen.primary or awful.screen.focused()
	State.dismiss_selected(s)
end

function M.scroll_up(s)
	if M.controller then
		M.controller.scroll_up(s)
	end
end

function M.scroll_down(s)
	if M.controller then
		M.controller.scroll_down(s)
	end
end

return M
