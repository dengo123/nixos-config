-- ~/.config/awesome/shell/launchers/lib/cancel.lua
-- Kompatibilitätsschicht zum alten API: mk_cancel_button(label, on_click[, style])
local Button = require("shell.launchers.lib.button")

local M = {}

function M.mk_cancel_button(label, on_click, style_override)
	-- nutzt deinen neuen Button mit fester Größe/Look
	return Button.mk_button(label or "Cancel", on_click, style_override)
end

return M
