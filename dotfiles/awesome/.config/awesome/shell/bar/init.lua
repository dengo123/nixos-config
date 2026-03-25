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

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return M.api or {}
end

local function bar_api()
	return api().bar or {}
end

local function bar_mod(name)
	return bar_api()[name]
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	M.api = {
		ui = args.ui or {},

		widgets = {
			clock = safe_require("shell.bar.widgets.clock"),
			layoutbox = safe_require("shell.bar.widgets.layoutbox"),
			notify = safe_require("shell.bar.widgets.notify"),
			start = safe_require("shell.bar.widgets.start"),
			systray = safe_require("shell.bar.widgets.systray"),
			tabs = safe_require("shell.bar.widgets.tabs"),
			tags = safe_require("shell.bar.widgets.tags"),
		},

		themes = {
			start = safe_require("shell.bar.themes.start"),
			tabs = safe_require("shell.bar.themes.tabs"),
			wibar = safe_require("shell.bar.themes.wibar"),
		},

		bar = {
			controller = safe_require("shell.bar.controller"),
			widgets = safe_require("shell.bar.widgets"),
			policy = safe_require("shell.bar.policy"),
			reveal = safe_require("shell.bar.reveal"),
			sections = safe_require("shell.bar.sections"),
		},
	}

	return M
end

function M.setup(s, args)
	local Controller = bar_mod("controller")

	if Controller and type(Controller.setup) == "function" then
		return Controller.setup({
			screen = s,
			args = args or {},
			api = api(),
		})
	end

	return nil
end

return M
