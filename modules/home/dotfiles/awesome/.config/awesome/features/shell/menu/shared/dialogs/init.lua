-- features/shell/menu/shared/dialogs/init.lua
local power = require("features.shell.menu.shared.dialogs.power")
local logout = require("features.shell.menu.shared.dialogs.logout")

return {
	power = power.power,
	logout_confirm = logout.logout_confirm,
}
