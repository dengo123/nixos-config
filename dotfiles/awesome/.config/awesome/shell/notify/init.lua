-- ~/.config/awesome/shell/notify/init.lua
local beautiful = require("beautiful")

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	api = {},
}

local initialized = false
local center_open = false
local center_signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function notify_cfg(cfg)
	return (cfg and cfg.notify) or {}
end

local function history_cfg(cfg)
	return notify_cfg(cfg).history or {}
end

local function emit_center_state()
	awesome.emit_signal("notify::center_state", center_open)
	awesome.emit_signal("ui::overlays_changed")
end

local function open_center()
	if center_open then
		return
	end

	center_open = true
	emit_center_state()
end

local function close_center()
	if not center_open then
		return
	end

	center_open = false
	emit_center_state()
end

local function toggle_center()
	center_open = not center_open
	emit_center_state()
end

local function register_center_signals()
	if center_signals_ready then
		return
	end

	center_signals_ready = true

	awesome.connect_signal("notify::toggle_center", toggle_center)
	awesome.connect_signal("notify::open_center", open_center)
	awesome.connect_signal("notify::close_center", close_center)
end

local function require_notify_theme()
	return beautiful.notify or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local cfg = args.cfg or args or {}
	local _ui = args.ui or {}

	if initialized then
		return M
	end

	M.api = {
		ui = _ui,
		theme = safe_require("shell.notify.theme"),
		naughty = safe_require("shell.notify.naughty"),
		pipeline = safe_require("shell.notify.pipeline"),
		center = safe_require("shell.notify.center"),
		history = safe_require("shell.notify.history"),
		rules = safe_require("shell.notify.rules"),
		shape = safe_require("shell.notify.shape"),
	}

	local Theme = mod("theme")
	local NaughtyConfig = mod("naughty")
	local Pipeline = mod("pipeline")
	local Center = mod("center")
	local History = mod("history")
	local Rules = mod("rules")
	local Shape = mod("shape")

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			cfg = cfg,
			ui = _ui,
		})
	end

	if NaughtyConfig and type(NaughtyConfig.init) == "function" then
		NaughtyConfig.init({
			cfg = cfg,
			shape = Shape,
			notify_theme = require_notify_theme(),
		})
	end

	if History and type(History.init) == "function" then
		History.init(history_cfg(cfg))
	end

	if Rules and type(Rules.apply) == "function" then
		Rules.apply()
	end

	if Center and type(Center.init) == "function" then
		Center.init({
			cfg = cfg,
			ui = _ui,
			history = History,
		})
	end

	if Pipeline and type(Pipeline.register) == "function" then
		Pipeline.register({
			cfg = cfg,
			history = History,
		})
	end

	register_center_signals()
	emit_center_state()

	initialized = true
	return M
end

function M.is_center_open()
	return center_open == true
end

function M.open_center()
	open_center()
end

function M.close_center()
	close_center()
end

function M.toggle_center()
	toggle_center()
end

return M
