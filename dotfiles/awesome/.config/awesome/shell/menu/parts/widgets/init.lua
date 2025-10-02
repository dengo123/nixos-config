local M = {}
M.cancel = require("shell.menu.parts.widgets.cancel")
M.icons = require("shell.menu.parts.widgets.icons")

-- bequeme short-hands
M.mk_cancel_button = M.cancel.mk_cancel_button
M.mk_icon_button = M.icons.mk_icon_button

return M
