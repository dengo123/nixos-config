-- shell/dialogs/lib/init.lua
local M = {}

local function safe_require(p)
	local ok, mod = pcall(require, p)
	return ok and mod or nil
end

-- Gemeinsame Bausteine
M.actions = safe_require("shell.dialogs.lib.actions")
M.cancel = safe_require("shell.dialogs.lib.cancel")
M.popup = safe_require("shell.dialogs.lib.popup")

-- Injection/Attach (optional flatten_actions)
function M.attach(api, opts)
	api = api or {}
	api.lib = M
	opts = opts or {}
	if opts.flatten_actions and M.actions then
		for k, v in pairs(M.actions) do
			if api[k] == nil then
				api[k] = v
			end
		end
	end
	return api
end

M.inject = M.attach
return M
