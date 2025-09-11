local awful = require("awful")
local M = {}

function M.build()
	return awful.widget.prompt()
end

return M
