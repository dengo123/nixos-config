-- ~/.config/awesome/shell/launchers/init.lua
local awful = require("awful")

local L = {
	lib = {},
	launchers = {},
	ui = {},
}

local runtime = {
	ctx = {},
	initialized = false,
}

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

local function ctx()
	return runtime.ctx or {}
end

local function cfg()
	return ctx().cfg or {}
end

local function ui_ctx()
	return ctx().ui or {}
end

local function lib(name)
	return L.lib[name]
end

local function launcher(name)
	return L.launchers[name]
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

local function resolve_theme(area, overrides)
	overrides = overrides or {}

	local ui = ui_ctx()
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
	if not runtime.initialized then
		L.init(ctx())
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

local function open_session_variant(opts, variant)
	ensure_init()

	opts = opts or {}
	opts.cfg = opts.cfg or L.cfg or {}
	opts.variant = variant

	local Session = launcher("session")
	if Session and type(Session.open) == "function" then
		return Session.open(opts, L)
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function L.init(args)
	if runtime.initialized then
		return L
	end

	runtime.ctx = args or {}

	local conf = cfg()
	local ui = ui_ctx()

	L.cfg = conf

	L.lib = {
		actions = safe_require("shell.launchers.lib.actions"),
		button = safe_require("shell.launchers.lib.button"),
		popup = safe_require("shell.launchers.lib.popup"),
	}

	L.launchers = {
		session = safe_require("shell.launchers.session"),
		run = safe_require("shell.launchers.run"),
	}

	L.ui = build_ui_bridge()

	local Button = lib("button")
	if Button and type(Button.init) == "function" then
		Button.init({
			ui = ui,
			cfg = conf,
		})
	end

	local Session = launcher("session")
	if Session and type(Session.init) == "function" then
		Session.init({
			ui = ui,
			cfg = conf,
			launchers = L,
		})
	end

	local Run = launcher("run")
	if Run and type(Run.init) == "function" then
		Run.init({
			ui = ui,
			cfg = conf,
			launchers = L,
		})
	end

	runtime.initialized = true
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
		return open_session_variant(opts, "power")
	end,

	logoff = function(opts)
		return open_session_variant(opts, "logoff")
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
