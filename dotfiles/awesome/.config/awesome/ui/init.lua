-- ~/.config/awesome/ui/init.lua
local theme = require("ui.theme")
local wallpaper = require("ui.wallpaper")

local M = {
	theme = theme,
	wallpaper = wallpaper,
}

--- Initialisiert UI (Theme zuerst, dann Wallpaper)
-- Nutzt .init(), fällt zurück auf .apply() bzw. .set()
-- @param cfg table: enthält u.a. .theme Pfad
function M.init(cfg)
	-- Theme
	if M.theme.init then
		M.theme.init(cfg)
	elseif M.theme.apply then
		M.theme.apply(cfg)
	end

	-- Wallpaper
	if M.wallpaper.init then
		M.wallpaper.init()
	else
		-- optional: einmalig global setzen, falls gewünscht
		-- wenn dein wallpaper-Modul nur .set(s) hat, reicht das hier meist aus:
		if M.wallpaper.set then
			-- kein Screen-Objekt -> Workspaces rufen set(s) ohnehin pro Screen auf
			-- also hier nichts tun
		end
	end
end

return M
