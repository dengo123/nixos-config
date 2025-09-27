-- features/shell/menu/dialogs/widgets/init.lua
local Header = require("features.shell.menu.dialogs.parts.widgets.header")
local Icons = require("features.shell.menu.dialogs.parts.widgets.icons")
local Cancel = require("features.shell.menu.dialogs.parts.widgets.cancel")
local RowsW = require("features.shell.menu.dialogs.parts.widgets.rows")

return {
	-- wie gehabt
	mk_header_content = Header.mk_header_content,

	-- getrennte Buttons
	buttons = {
		icons = Icons,
		cancel = Cancel,
	},

	-- Back-compat (falls alte Call-Sites noch existieren):
	mk_icon_button = Icons.mk_icon_button,
	mk_cancel_button = Cancel.mk_cancel_pill,

	-- rows-spezifische Widgets (falls du dort was brauchst)
	rows = RowsW,
}
