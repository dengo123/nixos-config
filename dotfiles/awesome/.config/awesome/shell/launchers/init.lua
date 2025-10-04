-- ~/.config/awesome/shell/launchers/init.lua
local awful = require("awful")

local L = {}

local function req(p)
	local ok, mod = pcall(require, p)
	assert(ok and mod, ("launchers/init: required module missing: %s"):format(p))
	return mod
end

-- Verbindliche, gemeinsame Infra (inkl. popup/actions/cancel)
L.lib = req("shell.launchers.lib")
-- Konkrete Launcher
L.power = req("shell.launchers.power")
L.run = req("shell.launchers.run")

-- ---------- Zentrale UI-API ----------
local function make_ui_api()
	local function resolve_theme(area, overrides)
		overrides = overrides or {}
		local ok_ui, ui = pcall(require, "ui")
		assert(ok_ui and ui and ui.theme and ui.theme[area], ("UI theme '%s' not initialized"):format(tostring(area)))
		local t = ui.theme[area]
		if type(t.resolve) == "function" then
			return t.resolve(overrides)
		end
		if type(t.get) == "function" then
			return t.get(overrides)
		end
		error(("ui.theme.%s has no resolve()/get()"):format(area))
	end

	local function resolve_screen(opts)
		return (opts and opts.screen) or (mouse and mouse.screen) or awful.screen.focused()
	end

	local function coalesce(v, d)
		return v == nil and d or v
	end

	local function open_panel(stack_widget, panel_theme, opts)
		opts = opts or {}
		local s = resolve_screen(opts)
		return L.lib.popup.show(stack_widget, panel_theme, {
			screen = s,
			width = assert(panel_theme.width, "panel width required"),
			height = assert(panel_theme.height, "panel height required"),
			use_backdrop = coalesce(opts.use_backdrop, true), -- false bleibt false
			close_on_backdrop = coalesce(opts.close_on_backdrop, false),
			show_root = coalesce(opts.show_root, "with_bars"), -- false bleibt false
			placement = coalesce(opts.placement, function(w)
				awful.placement.centered(w, { parent = s, honor_workarea = true })
			end),
			group = coalesce(opts.group, "launchers"),
		})
	end

	return { resolve_theme = resolve_theme, resolve_screen = resolve_screen, open_panel = open_panel }
end

L.ui_api = make_ui_api()
if L.lib.set_ui_api then
	L.lib.set_ui_api(L.ui_api)
else
	L.lib.ui_api = L.ui_api
end

-- Bequeme Direktaufrufe
L.open = {
	power = function(opts)
		if not L.lib.ui_api then
			L.lib.ui_api = L.ui_api
		end
		return L.power.open(opts, L.lib)
	end,
	run = function(opts)
		if not L.lib.ui_api then
			L.lib.ui_api = L.ui_api
		end
		return L.run.open(opts, L.lib)
	end,
}

return L
