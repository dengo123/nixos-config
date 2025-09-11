-- ~/.config/awesome/input/keys/global/menu.lua
local awful = require("awful")

return function(modkey)
	return awful.util.table.join(awful.key({ modkey }, "/", function()
		awesome.emit_signal("menu::toggle", { search = true })
	end, { description = "menu öffnen/schließen (mit suche)", group = "menu" }))
end
