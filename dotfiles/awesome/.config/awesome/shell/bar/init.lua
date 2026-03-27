-- ~/.config/awesome/shell/bar/init.lua
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
	last_setup_args = nil,
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

local function api()
	return M.api or {}
end

local function wibar_api()
	return api().wibar or {}
end

local function wibar_mod(name)
	return wibar_api()[name]
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = args or {}
	ensure_ctx_roots()

	local c = ctx()

	M.api = {
		root_ui = c.ui or {},

		widgets = {
			clock = safe_require("shell.bar.widgets.clock"),
			layoutbox = safe_require("shell.bar.widgets.layoutbox"),
			notify = safe_require("shell.bar.widgets.notify"),
			start = safe_require("shell.bar.widgets.start"),
			systray = safe_require("shell.bar.widgets.systray"),
			tabs = safe_require("shell.bar.widgets.tabs"),
			tags = safe_require("shell.bar.widgets.tags"),
		},

		bar_ui = {
			start = safe_require("shell.bar.ui.start"),
			tabs = safe_require("shell.bar.ui.tabs"),
			wibar = safe_require("shell.bar.ui.wibar"),
		},

		wibar = {
			controller = safe_require("shell.bar.wibar.controller"),
			widgets = safe_require("shell.bar.wibar.widgets"),
			sections = safe_require("shell.bar.wibar.sections"),
		},

		policy = safe_require("shell.bar.policy"),
		reveal = safe_require("shell.bar.reveal"),
	}

	c.shell.bar = M
	c.features.bar = M
	c.api.bar = M
	c.external.bar = M
	c.cfg.api.bar = M

	return M
end

function M.setup(s, args)
	local c = ctx()

	args = args
		or {
			cfg = c.cfg,
			ui = c.ui,
			menu_api = (c.features and c.features.menu) or (c.shell and c.shell.menu) or (c.api and c.api.menu),
		}

	runtime.last_setup_args = args

	local Controller = wibar_mod("controller")

	if Controller and type(Controller.setup) == "function" then
		return Controller.setup({
			screen = s,
			args = args,
			api = api(),
			ctx = c,
		})
	end

	return nil
end

function M.resync_all()
	local Controller = wibar_mod("controller")
	local args = runtime.last_setup_args or {}

	if not (Controller and type(Controller.setup) == "function") then
		return
	end

	for s in screen do
		Controller.setup({
			screen = s,
			args = args,
			api = api(),
			ctx = ctx(),
		})
	end
end

return M
