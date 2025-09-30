-- ~/.config/awesome/features/shell/menu/lib/init.lua
local M = {}

local ok_actions, Actions = pcall(require, "features.shell.menu.lib.actions")
local ok_apps, Apps = pcall(require, "features.shell.menu.lib.apps")
local ok_focus, Focus = pcall(require, "features.shell.menu.lib.focus")
local ok_helpers, Helpers = pcall(require, "features.shell.menu.lib.helpers")

M.actions = ok_actions and Actions or nil
M.apps = ok_apps and Apps or {}
M.focus = ok_focus and Focus or nil
M.helpers = ok_helpers and Helpers or nil

function M.attach(api, opts)
	opts = opts or {}
	api.lib = api.lib or {}
	api.lib.actions = M.actions
	api.lib.apps = M.apps
	api.lib.focus = M.focus
	api.lib.helpers = M.helpers

	if M.actions and type(M.actions.init) == "function" then
		M.actions.init(api)
	end

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

-- 🔁 Alias für bestehenden Code, der Lib.init(...) aufruft
M.init = M.attach

-- ✅ Passthrough: Actions an der Wurzel verfügbar machen
local passthrough = {
	"run",
	"run_dialog",
	"run_shell",
	"run_bin",
	"cmd",
	"lua",
	"signal",
	"click",
}
for _, k in ipairs(passthrough) do
	M[k] = function(...)
		if not M.actions or not M.actions[k] then
			return nil
		end
		return M.actions[k](...)
	end
end

function M.defaults()
	return M.apps or {}
end

return M
