-- ~/.config/awesome/shell/launchers/session/init.lua
local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

local M = {
	container = nil,
	layout = nil,
	icons = nil,
	theme = nil,
	variants = {},
	controller_mod = nil,
	controller = nil,
}

local runtime = {
	cfg = {},
	ui = {},
	launchers = nil,
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

local function launchers()
	return runtime.launchers
end

local function resolve_lib()
	local L = launchers() or {}
	return L.lib or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	runtime.cfg = opts.cfg or runtime.cfg
	runtime.ui = opts.ui or runtime.ui
	runtime.launchers = opts.launchers or runtime.launchers

	M.container = safe_require("shell.launchers.session.container")
	M.layout = safe_require("shell.launchers.session.layout")
	M.icons = safe_require("shell.launchers.session.icons")
	M.theme = safe_require("shell.launchers.session.theme")
	M.controller_mod = safe_require("shell.launchers.session.controller")

	M.variants = {
		logoff = safe_require("shell.launchers.session.logoff"),
		power = safe_require("shell.launchers.session.power"),
	}

	if M.controller_mod and type(M.controller_mod.new) == "function" then
		M.controller = M.controller_mod.new({
			container = M.container,
			layout = M.layout,
			icons = M.icons,
			theme = M.theme,
			variants = M.variants,
			resolve_lib = resolve_lib,
			cfg = cfg,
			ui = ui,
		})
	end

	return M
end

function M.is_open()
	local Controller = M.controller
	return Controller and type(Controller.is_open) == "function" and Controller.is_open() or false
end

function M.close()
	local Controller = M.controller
	if Controller and type(Controller.close) == "function" then
		Controller.close()
	end
end

function M.open(opts)
	local Controller = M.controller
	if Controller and type(Controller.open) == "function" then
		return Controller.open(opts or {})
	end

	return nil
end

return M
