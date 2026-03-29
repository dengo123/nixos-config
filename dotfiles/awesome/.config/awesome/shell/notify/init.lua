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
	ui = {},
	runtime = nil,
	policy = {},
	center = nil,
	theme = {},
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

local function cfg()
	return ctx().cfg or {}
end

local function ui_ctx()
	return ctx().ui or {}
end

local function shell()
	return ctx().shell or {}
end

local function emit_center_state()
	awesome.emit_signal("notify::center_state", runtime.center_open)
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

local function build_notify_theme(Theme)
	if not Theme then
		return {}
	end

	if type(Theme.build) == "function" then
		local built = Theme.build({
			cfg = cfg(),
			ui = ui_ctx(),
		})

		if type(Theme.apply_to_beautiful) == "function" then
			Theme.apply_to_beautiful(built or {})
		elseif type(Theme.init) == "function" then
			Theme.init({
				cfg = cfg(),
				ui = ui_ctx(),
			})
		end

		return (built and built.notify) or {}
	end

	if type(Theme.init) == "function" then
		Theme.init({
			cfg = cfg(),
			ui = ui_ctx(),
		})
	end

	return beautiful.notify or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}

	if runtime.initialized then
		return M
	end

	M.ui = {
		theme = safe_require("shell.notify.ui.theme"),
		naughty = safe_require("shell.notify.ui.naughty"),
		shape = safe_require("shell.notify.ui.shape"),
	}

	M.runtime = safe_require("shell.notify.runtime.init")

	M.policy = {
		rules = safe_require("shell.notify.policy.rules"),
	}

	M.center = safe_require("shell.notify.center")

	local Theme = M.ui.theme
	local NaughtyConfig = M.ui.naughty
	local Shape = M.ui.shape

	local NotifyRuntime = M.runtime
	local Rules = M.policy.rules
	local Center = M.center

	M.theme = build_notify_theme(Theme)

	if NaughtyConfig and type(NaughtyConfig.init) == "function" then
		NaughtyConfig.init({
			cfg = cfg(),
			shape = Shape,
			notify_theme = M.theme or {},
		})
	end

	if Rules and type(Rules.apply) == "function" then
		Rules.apply()
	end

	if NotifyRuntime and type(NotifyRuntime.init) == "function" then
		NotifyRuntime.init({
			cfg = cfg(),
			ui = ui_ctx(),
			shell = shell(),
			notify = M,
			notify_theme = M.theme or {},
		})
	end

	if NotifyRuntime and type(NotifyRuntime.restore) == "function" then
		NotifyRuntime.restore()
	end

	if Center and type(Center.init) == "function" then
		Center.init({
			cfg = cfg(),
			ui = ui_ctx(),
			history = NotifyRuntime and NotifyRuntime.history or nil,
			notify = M,
			notify_theme = M.theme or {},
			shell = shell(),
		})
	end

	if NotifyRuntime and type(NotifyRuntime.attach_signals) == "function" then
		NotifyRuntime.attach_signals()
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
