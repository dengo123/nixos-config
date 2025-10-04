-- ~/.config/awesome/shell/launchers/init.lua
-- Zentrale Launcher-API:
-- - Stellt eine einheitliche UI-API bereit (resolve_theme/screen, open_panel)
-- - Injiziert dieselbe Lib (inkl. Popup) an alle Launcher (power, run, …)
-- - Keine Key/Theme-Logik hier – nur Infrastruktur

local awful = require("awful")

local L = {}

local function safe_require(p)
	local ok, mod = pcall(require, p)
	return ok and mod or nil
end

-- Gemeinsame Infra (Lib) laden
L.lib = safe_require("shell.launchers.lib") or {}

-- Stelle sicher, dass die gemeinsame Popup-Lib existiert (einzige Quelle)
do
	if not L.lib.popup then
		local popup = safe_require("shell.launchers.lib.popup")
		assert(popup and popup.show, "launchers/init: shell.launchers.lib.popup fehlt oder exportiert kein .show()")
		L.lib.popup = popup
	end
end

-- Konkrete Launcher
L.power = safe_require("shell.launchers.power")
L.run = safe_require("shell.launchers.run")

-- ---------- Zentrale UI-API (einheitlich für alle Launcher) ----------
local function make_ui_api()
	local function resolve_theme(area, overrides)
		-- 1) Versuche über ui.theme.<area>, falls ui schon geladen ist
		local ok_ui, ui = pcall(require, "ui")
		if ok_ui and ui and ui.theme and ui.theme[area] then
			local t = ui.theme[area]
			if type(t.resolve) == "function" then
				return t.resolve(overrides or {})
			elseif type(t.get) == "function" then
				return t.get(overrides or {})
			end
		end
		-- 2) Fallback: direktes Modul ui.theme.<area>
		local mod = safe_require("ui.theme." .. tostring(area))
		if mod then
			if type(mod.resolve) == "function" then
				return mod.resolve(overrides or {})
			elseif type(mod.get) == "function" then
				return mod.get(overrides or {})
			end
		end
		error(("launchers.ui_api: kein Theme-Resolver für '%s' gefunden"):format(tostring(area)))
	end

	local function resolve_screen(opts)
		return (opts and opts.screen) or (mouse and mouse.screen) or awful.screen.focused()
	end

	local function open_panel(stack_widget, panel_theme, opts)
		opts = opts or {}
		local s = resolve_screen(opts)
		local Popup = L.lib.popup
		-- Popup.show erwartet: (widget, themeTable, {screen,width,height,...})
		return Popup.show(stack_widget, panel_theme, {
			screen = s,
			width = assert(panel_theme.width, "panel width required"),
			height = assert(panel_theme.height, "panel height required"),
			use_backdrop = (opts.use_backdrop ~= false),
			show_root = opts.show_root or "with_bars",
			placement = opts.placement or function(w)
				awful.placement.centered(w, { parent = s, honor_workarea = true })
			end,
			group = opts.group or "launchers",
		})
	end

	return {
		resolve_theme = resolve_theme,
		resolve_screen = resolve_screen,
		open_panel = open_panel,
	}
end

L.ui_api = make_ui_api()
-- Optional: auch an die Lib hängen, falls Launcher dort nachschauen
if L.lib.set_ui_api then
	L.lib.set_ui_api(L.ui_api)
else
	L.lib.ui_api = L.ui_api
end

-- Bequeme Direktaufrufe mit identischer Lib + UI-API
L.open = {
	power = function(opts)
		-- gleiche Lib für alle Launcher
		if L.lib and not L.lib.ui_api then
			L.lib.ui_api = L.ui_api
		end
		return L.power and L.power.open(opts, L.lib)
	end,
	run = function(opts)
		if L.lib and not L.lib.ui_api then
			L.lib.ui_api = L.ui_api
		end
		return L.run and L.run.open(opts, L.lib)
	end,
}

return L
