-- ~/.config/awesome/features/shell/menu/init.lua
local core = require("features.shell.menu.core")
local defaults = require("features.shell.menu.data")

local M = {}

-- opts:
--   cfg            = {} (durchgereicht)
--   theme          = table ODER function()->table (z. B. require("ui.theme").startmenu)
--   data           = { user={}, left_items={}, right_items={}, power_items={} }
-- Rückgabe: { menu = api(:show/:hide/:toggle), launcher = nil }
function M.create(opts)
	opts = opts or {}

	-- Theme auflösen (Funktion oder Tabelle akzeptieren)
	local theme = opts.theme
	if type(theme) == "function" then
		theme = theme()
	end
	if type(theme) ~= "table" then
		-- Fallback: versuche ui.theme.startmenu(), sonst minimale Defaults
		local ok, tmod = pcall(require, "ui.theme")
		if ok and type(tmod.startmenu) == "function" then
			theme = tmod.startmenu()
		elseif ok and type(tmod) == "table" then
			theme = tmod
		else
			theme = {
				bg = "#222222",
				fg = "#ffffff",
				border_color = "#000000",
				border_width = 1,
				total_height = 500,
			}
		end
	end

	-- Daten mergen (User kann nur Teilbereiche liefern)
	local data = {}
	local src = opts.data or {}
	local function pick(k)
		return (src[k] ~= nil) and src[k] or defaults[k]
	end
	data.user = pick("user")
	data.left_items = pick("left_items")
	data.right_items = pick("right_items")
	data.power_items = pick("power_items")

	-- Popup + API bauen
	local build_popup = (type(core) == "function") and core or core.build_popup
	assert(type(build_popup) == "function", "menu.core export mismatch: expected function 'build_popup'")

	local api = build_popup({
		theme = theme,
		data = data,
	})

	return { menu = api, launcher = nil }
end

return M
