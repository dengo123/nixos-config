-- features/shell/menu/lib/init.lua
local M = {}

-- Einzel-Module einsammeln
local ok_actions, Actions = pcall(require, "features.shell.menu.lib.actions")
local ok_apps, Apps = pcall(require, "features.shell.menu.lib.apps")
local ok_focus, Focus = pcall(require, "features.shell.menu.lib.focus")
local ok_helpers, Helpers = pcall(require, "features.shell.menu.lib.helpers")

M.actions = ok_actions and Actions or nil
M.apps = ok_apps and Apps or {}
M.focus = ok_focus and Focus or nil
M.helpers = ok_helpers and Helpers or nil

--- Hängt alle Lib-Funktionen sauber an die Menü-API.
-- @param api table  -> das von parts.build_popup() gelieferte Menü-API
-- @param opts table -> { flatten_helpers=true, flatten_focus=true }
function M.attach(api, opts)
	opts = opts or {}

	api.lib = api.lib or {}
	api.lib.actions = M.actions
	api.lib.apps = M.apps
	api.lib.focus = M.focus
	api.lib.helpers = M.helpers

	-- Actions-Dispatcher initialisieren (falls vorhanden)
	if M.actions and type(M.actions.init) == "function" then
		M.actions.init(api)
	end

	-- Optional: ausgewählte Helfer flach direkt ans API hängen
	if opts.flatten_helpers and M.helpers then
		for k, v in pairs(M.helpers) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	if opts.flatten_focus and M.focus then
		for k, v in pairs(M.focus) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end

	return api
end

--- Liefert das Defaults-Table (für merge_data usw.).
function M.defaults()
	return M.apps or {}
end

return M
