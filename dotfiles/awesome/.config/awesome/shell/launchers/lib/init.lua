-- ~/.config/awesome/shell/launchers/lib/init.lua
local M = {}

local function req(p)
	local ok, mod = pcall(require, p)
	assert(ok and mod, p)
	return mod
end

M.actions = req("shell.launchers.lib.actions")
M.cancel = req("shell.launchers.lib.cancel")
M.button = req("shell.launchers.lib.button")
M.popup = req("shell.launchers.lib.popup")
assert(type(M.popup.show) == "function", "popup.show required")

function M.set_ui_api(ui_api)
	M.ui_api = ui_api
	return M
end

function M.attach(api)
	api = api or {}
	api.lib = M
	return api
end

M.inject = M.attach

return M
