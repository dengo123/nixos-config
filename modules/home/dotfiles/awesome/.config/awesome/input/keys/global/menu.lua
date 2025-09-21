-- ~/.config/awesome/input/keys/global/menu.lua
local awful = require("awful")

local function call_api(action)
	local api = rawget(_G, "__menu_api")
	if not api then
		return
	end

	local function open_menu()
		if api.show then
			api.show()
		elseif api.toggle then
			api.toggle()
		end
	end

	if action == "search_local" then
		open_menu()
		if api.focus_search then
			api.focus_search()
		end
	elseif action == "search_web" then
		open_menu()
		if api.focus_search_web then
			api.focus_search_web()
		else
			-- Fallback direkt auf die Search-API
			local s = rawget(_G, "__search_api")
			if s and s.focus_web then
				s.focus_web()
			end
		end
	elseif action == "toggle" then
		if api.toggle then
			api.toggle()
		end
	end
end

return function(modkey)
	local join = awful.util.table.join
	local function bind(mods, key, fn, desc)
		return awful.key(mods, key, fn, { description = desc, group = "menu" })
	end

	return join(
		-- Super + / -> Menü + lokale Suche
		bind({ modkey }, "/", function()
			call_api("search_local")
		end, "open menu + local search"),
		bind({ modkey }, "slash", function()
			call_api("search_local")
		end, "open menu + local search"),

		-- Super + Shift + / -> Menü + Websuche
		bind({ modkey, "Shift" }, "/", function()
			call_api("search_web")
		end, "open menu + web search"),
		bind({ modkey, "Shift" }, "question", function()
			call_api("search_web")
		end, "open menu + web search"),

		-- Super + Space -> nur Menü toggeln
		bind({ modkey }, "space", function()
			call_api("toggle")
		end, "toggle menu")
	)
end
