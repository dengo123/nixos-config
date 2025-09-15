-- features/shell/menu/shared/dialogs/init.lua
local power = require("features.shell.menu.dialogs.power")
local logout = require("features.shell.menu.dialogs.logout")

return {
	power = power.power,
	logout_confirm = logout.logout_confirm,
}
