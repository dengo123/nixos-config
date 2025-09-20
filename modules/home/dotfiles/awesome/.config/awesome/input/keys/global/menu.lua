-- ~/.config/awesome/input/keys/global/menu.lua
local awful = require("awful")

local function call_api(action)
	local api = rawget(_G, "__menu_api")
	if not api then
		return
	end

	if action == "search_local" then
		if api.toggle then
			api.toggle()
		end
		if api.focus_search then
			api.focus_search()
		end
	elseif action == "search_web" then
		if api.toggle then
			api.toggle()
		end
		if api.focus_search_web then
			api.focus_search_web()
		end
	elseif action == "toggle" then
		if api.toggle then
			api.toggle()
		end
	end
end

return function(modkey)
	return awful.util.table.join(
		-- Super + / → Menü öffnen + lokale Suche
		awful.key({ modkey }, "/", function()
			call_api("search_local")
		end, { description = "open menu + local search", group = "menu" }),

		-- Super + Shift + / → Menü öffnen + Websuche
		awful.key({ modkey, "Shift" }, "/", function()
			call_api("search_web")
		end, { description = "open menu + web search", group = "menu" }),

		-- Super + Space → Menü nur toggeln
		awful.key({ modkey }, "space", function()
			call_api("toggle")
		end, { description = "toggle menu", group = "menu" })
	)
end
