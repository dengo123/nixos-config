-- ~/.config/awesome/shell/launchers/init.lua
local awful = require("awful")

local L = {}

local initialized = false

local function req(path)
	local ok, mod = pcall(require, path)
	if not ok then
		error(("launchers/init: require failed: %s\n%s"):format(path, tostring(mod)))
	end
	if not mod then
		error(("launchers/init: module returned nil: %s"):format(path))
	end
	return mod
end

L.actions = req("shell.launchers.lib.actions")
L.button = req("shell.launchers.lib.button")
L.popup = req("shell.launchers.lib.popup")

L.power = req("shell.launchers.power")
L.run = req("shell.launchers.run")

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolve_theme(area, overrides)
	overrides = overrides or {}

	local ok_ui, ui = pcall(require, "ui")
	assert(ok_ui and ui and ui.theme and ui.theme[area], ("UI theme '%s' not initialized"):format(tostring(area)))

	local theme = ui.theme[area]

	if type(theme.resolve) == "function" then
		return theme.resolve(overrides)
	end

	if type(theme.get) == "function" then
		return theme.get(overrides)
	end

	error(("ui.theme.%s has no resolve()/get()"):format(area))
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

	return L.popup.show(stack_widget, panel_theme, {
		screen = s,
		width = assert(panel_theme.width, "panel width required"),
		height = assert(panel_theme.height, "panel height required"),
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

local function build_ui_api()
	return {
		resolve_theme = resolve_theme,
		resolve_screen = resolve_screen,
		open_panel = open_panel,
	}
end

local function ensure_init()
	if not initialized then
		L.init()
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function L.init()
	if initialized then
		return L
	end

	-- ---------------------------------------------------------------------
	-- Shared Infra
	-- ---------------------------------------------------------------------

	L.ui_api = build_ui_api()
	assert(type(L.popup.show) == "function", "launchers.popup.show required")

	initialized = true
	return L
end

L.open = {
	power = function(opts)
		ensure_init()
		return L.power.open(opts, L)
	end,

	run = function(opts)
		ensure_init()
		return L.run.open(opts, L)
	end,
}

return L
