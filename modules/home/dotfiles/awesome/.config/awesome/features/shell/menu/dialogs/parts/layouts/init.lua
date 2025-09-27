-- features/shell/menu/dialogs/parts/layouts/init.lua
local Util = require("features.shell.menu.dialogs.parts.layouts.util")
local I = require("features.shell.menu.dialogs.parts.layouts.icons")
local R = require("features.shell.menu.dialogs.parts.layouts.rows")

return {
	-- shared
	compute_metrics = Util.compute_metrics,
	-- classic
	actions_row = I.actions_row,
	actions_grid = I.actions_grid,
	-- rows view
	rows_view = R.rows_view,
	make_icon_buttons = R.make_icon_buttons,
}
