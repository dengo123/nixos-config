-- ~/.config/awesome/shell/bar/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	widgets = {},
	ui = {},
	wibar = {},
	policy = nil,
	behavior = {},
}

local runtime = {
	cfg = {},
	ui = {},
	shell = {},
	last_setup_args = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function cfg()
	return runtime.cfg or {}
end

local function ui()
	return runtime.ui or {}
end

local function shell()
	return runtime.shell or {}
end

local function menu()
	return shell().menu
end

local function notify()
	return shell().notify
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	runtime.cfg = opts.cfg or runtime.cfg
	runtime.ui = opts.ui or runtime.ui
	runtime.shell = opts.shell or runtime.shell

	M.widgets = {
		clock = safe_require("shell.bar.widgets.clock"),
		layoutbox = safe_require("shell.bar.widgets.layoutbox"),
		notify = safe_require("shell.bar.widgets.notify"),
		start = safe_require("shell.bar.widgets.start"),
		systray = safe_require("shell.bar.widgets.systray"),
		tabs = safe_require("shell.bar.widgets.tabs"),
		tags = safe_require("shell.bar.widgets.tags"),
	}

	M.ui = {
		start = safe_require("shell.bar.ui.start"),
		tabs = safe_require("shell.bar.ui.tabs"),
		wibar = safe_require("shell.bar.ui.wibar"),
	}

	M.wibar = {
		controller = safe_require("shell.bar.wibar.controller"),
		widgets = safe_require("shell.bar.wibar.widgets"),
		sections = safe_require("shell.bar.wibar.sections"),
	}

	M.policy = safe_require("shell.bar.policy")
	M.behavior = {
		reveal = safe_require("shell.bar.behavior.reveal"),
		group_tabs = safe_require("shell.bar.behavior.group_tabs"),
	}

	return M
end

function M.setup(s, opts)
	opts = opts or {
		cfg = cfg(),
		ui = ui(),
		menu = menu(),
		notify = notify(),
	}

	runtime.last_setup_args = opts

	local Controller = M.wibar.controller
	if Controller and type(Controller.setup) == "function" then
		return Controller.setup({
			screen = s,
			args = opts,
			bar = M,
		})
	end

	return nil
end

function M.resync_all()
	local Controller = M.wibar.controller
	local opts = runtime.last_setup_args or {
		cfg = cfg(),
		ui = ui(),
		menu = menu(),
		notify = notify(),
	}

	if not (Controller and type(Controller.setup) == "function") then
		return
	end

	for s in screen do
		Controller.setup({
			screen = s,
			args = opts,
			bar = M,
		})
	end
end

return M
