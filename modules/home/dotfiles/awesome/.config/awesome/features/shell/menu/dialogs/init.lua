local Power = require("features.shell.menu.dialogs.power")
local Confirm = require("features.shell.menu.dialogs.confirm")

local M = {}

function M.power(theme)
	Power.open(theme)
end

function M.logout_confirm(theme)
	Confirm.logoff(theme)
end

function M.confirm(args)
	Confirm.open(args)
end

return M
