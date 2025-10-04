-- ~/.config/awesome/shell/launchers/lib/init.lua
-- Zentrale Helfer für alle Launcher (power/run/…):
-- - actions / cancel  : optionale Bausteine
-- - popup             : gemeinsame Popup-Implementierung
-- - ui_api (optional) : wird extern gesetzt, z. B. über launchers/init.lua
--
-- Keine harten Requires auf Themes oder UI hier!

local M = {}

local function safe_require(p)
	local ok, mod = pcall(require, p)
	return ok and mod or nil
end

-- Optionale Bausteine (falls vorhanden)
M.actions = safe_require("shell.dialogs.lib.actions")
M.cancel = safe_require("shell.dialogs.lib.cancel")
M.popup = safe_require("shell.launchers.lib.popup")

-- Optional: UI-API anheften (wird i. d. R. in launchers/init.lua gesetzt)
-- Erwartete Felder auf ui_api (wenn genutzt):
--   - resolve_theme(area, overrides)
--   - resolve_screen(opts)
--   - open_panel(widget, panel_theme, opts)
function M.set_ui_api(ui_api)
	M.ui_api = ui_api
	return M
end

-- Injection/Attach: Hilfsfunktion um diese Lib an ein externes API-Objekt zu hängen.
-- opts.flatten_actions = true  -> exportiere actions.* als direkte Methoden, falls frei
function M.attach(api, opts)
	api = api or {}
	opts = opts or {}

	api.lib = M

	if opts.flatten_actions and M.actions then
		for k, v in pairs(M.actions) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	return api
end

-- Back-compat alias
M.inject = M.attach

return M
