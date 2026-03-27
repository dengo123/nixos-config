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

local runtime = {
	ctx = {},
	initialized = false,
	center_open = false,
	center_signals_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function ensure_ctx_roots()
	local c = ctx()

	c.shell = c.shell or {}
	c.features = c.features or {}
	c.api = c.api or {}
	c.external = c.external or {}
	c.cfg = c.cfg or {}
	c.cfg.api = c.cfg.api or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui()
	return ctx().ui or {}
end

local function api()
	return M.api or {}
end

local function mod(name)
	return api()[name]
end

local function notify_cfg(conf)
	return (conf and conf.notify) or {}
end

local function history_cfg(conf)
	return notify_cfg(conf).history or {}
end

local function emit_center_state()
	awesome.emit_signal("notify::center_state", runtime.center_open)
	awesome.emit_signal("ui::overlays_changed")
end

local function open_center()
	if runtime.center_open then
		return
	end

	runtime.center_open = true
	emit_center_state()
end

local function close_center()
	if not runtime.center_open then
		return
	end

	runtime.center_open = false
	emit_center_state()
end

local function toggle_center()
	runtime.center_open = not runtime.center_open
	emit_center_state()
end

local function register_center_signals()
	if runtime.center_signals_ready then
		return
	end

	runtime.center_signals_ready = true

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
	runtime.ctx = args or {}
	ensure_ctx_roots()

	local c = ctx()

	if runtime.initialized then
		return M
	end

	M.api = {
		ui = c.ui or {},
		theme = safe_require("shell.notify.ui.theme"),
		naughty = safe_require("shell.notify.ui.naughty"),
		pipeline = safe_require("shell.notify.runtime.pipeline"),
		center = safe_require("shell.notify.center"),
		history = safe_require("shell.notify.runtime.history"),
		rules = safe_require("shell.notify.policy.rules"),
		shape = safe_require("shell.notify.ui.shape"),
	}

	c.shell.notify = M
	c.features.notify = M
	c.api.notify = M
	c.external.notify = M
	c.cfg.api.notify = M

	local Theme = mod("theme")
	local NaughtyConfig = mod("naughty")
	local Pipeline = mod("pipeline")
	local Center = mod("center")
	local History = mod("history")
	local Rules = mod("rules")
	local Shape = mod("shape")

	if Theme and type(Theme.init) == "function" then
		Theme.init({
			ctx = c,
			cfg = cfg(),
			ui = ui(),
		})
	end

	if NaughtyConfig and type(NaughtyConfig.init) == "function" then
		NaughtyConfig.init({
			ctx = c,
			cfg = cfg(),
			shape = Shape,
			notify_theme = require_notify_theme(),
		})
	end

	if History and type(History.init) == "function" then
		History.init(history_cfg(cfg()))
	end

	if Rules and type(Rules.apply) == "function" then
		Rules.apply()
	end

	if Center and type(Center.init) == "function" then
		Center.init({
			ctx = c,
			cfg = cfg(),
			ui = ui(),
			history = History,
		})
	end

	if Pipeline and type(Pipeline.init) == "function" then
		Pipeline.init({
			ctx = c,
		})
	end

	if Pipeline and type(Pipeline.register) == "function" then
		Pipeline.register({
			ctx = c,
			cfg = cfg(),
			history = History,
		})
	end

	register_center_signals()
	emit_center_state()

	runtime.initialized = true
	return M
end

function M.is_center_open()
	return runtime.center_open == true
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
