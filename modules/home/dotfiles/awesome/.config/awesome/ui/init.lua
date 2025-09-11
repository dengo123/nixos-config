-- ~/.config/awesome/ui/init.lua
local M = {
	theme = require("ui.theme"),
	wallpaper = require("ui.wallpaper"),
}

--- Initialisiert UI (Theme zuerst, dann Wallpaper)
-- @param cfg table: enthält u.a. .theme Pfad
function M.init(cfg)
	M.theme.init(cfg)
	M.wallpaper.init()
end

return M
