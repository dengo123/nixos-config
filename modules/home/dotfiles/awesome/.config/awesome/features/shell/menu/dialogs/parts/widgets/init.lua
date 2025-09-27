-- features/shell/menu/dialogs/parts/widgets/init.lua
local Header = require("features.shell.menu.dialogs.parts.widgets.header")
local Buttons = require("features.shell.menu.dialogs.parts.widgets.icons")
local Cancel = require("features.shell.menu.dialogs.parts.widgets.cancel")
-- local RowsW = require("features.shell.menu.dialogs.parts.widgets.rows")

local W = {}

-- Alte, öffentliche API kompatibel halten:
W.mk_header_content = Header.mk_header_content
W.mk_icon_button = Buttons.mk_icon_button
W.mk_cancel_button = Cancel.mk_cancel_button

-- Neue strukturierte Unterbäume (optional nutzbar)
W.header = Header
W.buttons = Buttons
W.cancel = Cancel
-- W.rows = RowsW

return W
