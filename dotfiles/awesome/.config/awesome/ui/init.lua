-- ~/.config/awesome/ui/init.lua
local M = { theme = require("ui.theme"), wallpaper = require("ui.wallpaper") }
function M.init(cfg)
	if M.theme and M.theme.init then
		M.theme.init(cfg)
	end
	if M.wallpaper and M.wallpaper.init then
		M.wallpaper.init(cfg)
	end
	return M
end

return M
