-- ~/.config/awesome/shell/launchers/init.lua
local awful = require("awful")

local L = {
	api = {},
}

local initialized = false
local runtime_cfg = {}

local function safe_require(path)
	local ok, mod = pcall(require, path)
	if ok then
		return mod
	end

	return nil
end

-- =========================================================================
-- Helpers
-- =========================================================================

local function api()
	return L.api or {}
end

local function lib_api()
	return api().lib or {}
end

local function launcher_api()
	return api().launchers or {}
end

local function ui_api()
	return api().ui or {}
end

local function lib(name)
	return lib_api()[name]
end

local function launcher(name)
	return launcher_api()[name]
end

local function launcher_modules()
	return {
		{
			key = "session",
			mod = launcher("session"),
		},
		{
			key = "run",
			mod = launcher("run"),
		},
	}
end

local function merge_shallow(a, b)
	local out = {}

	for k, v in pairs(a or {}) do
		out[k] = v
	end

	for k, v in pairs(b or {}) do
		out[k] = v
	end

	return out
end

local function resolve_ui(args, cfg)
	local arg_ui = (args and args.ui) or {}
	local cfg_ui = (cfg and cfg.ui) or {}
	local api_ui = (cfg and cfg.api and cfg.api.ui) or {}

	local ui = merge_shallow(cfg_ui, api_ui)
	ui = merge_shallow(ui, arg_ui)

	if not (ui.theme and ui.theme.colors and ui.theme.fonts) then
		local UI = require("ui")
		local ui_mod = UI.init({ cfg = cfg or {} })
		ui = ui_mod.get()
	end

	if ui.colors == nil then
		local ok, mod = pcall(require, "ui.colors")
		if ok and mod then
			ui.colors = mod
		end
	end

	if ui.helpers == nil then
		local ok, mod = pcall(require, "ui.helpers")
		if ok and mod then
			ui.helpers = mod
		end
	end

	return ui
end

local function resolve_theme(area, overrides)
	overrides = overrides or {}

	local ui = ui_api()
	local theme_root = ui.theme or {}
	local theme = theme_root[area]

	assert(theme, ("launchers.init: ui theme '%s' fehlt"):format(tostring(area)))

	if type(theme.resolve) == "function" then
		return theme.resolve(overrides)
	end

	if type(theme.get) == "function" then
		return theme.get(overrides)
	end

	error(("launchers.init: ui theme '%s' hat kein resolve()/get()"):format(tostring(area)))
end

local function resolve_screen(opts)
	return (opts and opts.screen) or (mouse and mouse.screen) or awful.screen.focused()
end

local function coalesce(value, default)
	return value == nil and default or value
end

local function open_panel(stack_widget, panel_theme, opts)
	opts = opts or {}

	local s = resolve_screen(opts)
	local Popup = lib("popup")

	assert(Popup and type(Popup.show) == "function", "launchers.init: popup.show fehlt")

	return Popup.show(stack_widget, panel_theme, {
		screen = s,
		width = panel_theme.width,
		height = panel_theme.height,
		use_backdrop = coalesce(opts.use_backdrop, true),
		close_on_backdrop = coalesce(opts.close_on_backdrop, false),
		show_root = coalesce(opts.show_root, "with_bars"),
		placement = coalesce(opts.placement, function(w)
			awful.placement.centered(w, { parent = s, honor_workarea = true })
		end),
		group = coalesce(opts.group, "launchers"),
		shape = opts.shape,
	})
end

local function build_ui_bridge()
	return {
		resolve_theme = resolve_theme,
		resolve_screen = resolve_screen,
		open_panel = open_panel,
	}
end

local function ensure_init()
	if not initialized then
		L.init(runtime_cfg)
	end
end

local function safe_is_open(mod)
	return mod and type(mod.is_open) == "function" and mod.is_open() == true
end

local function safe_close(mod)
	if mod and type(mod.close) == "function" then
		mod.close()
	end
end

local function each_launcher(fn)
	for _, entry in ipairs(launcher_modules()) do
		fn(entry.key, entry.mod)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function L.init(args)
	if initialized then
		return L
	end

	args = args or {}

	runtime_cfg = args.cfg or args or {}
	L.cfg = runtime_cfg

	local resolved_ui = resolve_ui(args, runtime_cfg)

	L.api = {
		lib = {
			actions = safe_require("shell.launchers.lib.actions"),
			button = safe_require("shell.launchers.lib.button"),
			popup = safe_require("shell.launchers.lib.popup"),
		},
		launchers = {
			session = safe_require("shell.launchers.session"),
			run = safe_require("shell.launchers.run"),
		},
		ui = resolved_ui,
	}

	local Button = lib("button")
	if Button and type(Button.init) == "function" then
		Button.init({
			ui = api().ui or {},
		})
	end

	local Session = launcher("session")
	if Session and type(Session.init) == "function" then
		Session.init({
			ui = api().ui or {},
		})
	end

	local Run = launcher("run")
	if Run and type(Run.init) == "function" then
		Run.init({
			ui = api().ui or {},
		})
	end

	L.ui_api = build_ui_bridge()

	initialized = true
	return L
end

function L.is_any_open()
	ensure_init()

	local open = false

	each_launcher(function(_, mod)
		if safe_is_open(mod) then
			open = true
		end
	end)

	return open
end

function L.close_all()
	ensure_init()

	each_launcher(function(_, mod)
		safe_close(mod)
	end)
end

function L.build_overlays()
	ensure_init()

	local overlays = {}

	each_launcher(function(_, mod)
		table.insert(overlays, {
			is_open = function()
				return safe_is_open(mod)
			end,
			close = function()
				safe_close(mod)
			end,
		})
	end)

	return overlays
end

L.open = {
	power = function(opts)
		ensure_init()
		opts = opts or {}
		opts.cfg = opts.cfg or L.cfg or {}
		opts.variant = "power"

		local Session = launcher("session")
		if Session and type(Session.open) == "function" then
			return Session.open(opts, L)
		end
	end,

	logoff = function(opts)
		ensure_init()
		opts = opts or {}
		opts.cfg = opts.cfg or L.cfg or {}
		opts.variant = "logoff"

		local Session = launcher("session")
		if Session and type(Session.open) == "function" then
			return Session.open(opts, L)
		end
	end,

	session = function(opts)
		ensure_init()
		opts = opts or {}
		opts.cfg = opts.cfg or L.cfg or {}

		local Session = launcher("session")
		if Session and type(Session.open) == "function" then
			return Session.open(opts, L)
		end
	end,

	run = function(opts)
		ensure_init()
		opts = opts or {}
		opts.cfg = opts.cfg or L.cfg or {}

		local Run = launcher("run")
		if Run and type(Run.open) == "function" then
			return Run.open(opts, L)
		end
	end,
}

return L
