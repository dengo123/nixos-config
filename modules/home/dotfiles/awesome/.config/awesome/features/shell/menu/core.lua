-- features/shell/menu/core.lua
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local placement = require("features.shell.menu.shared.placement")

local M = {}

-- args = { theme=table, data={ user, left_items, right_items, power_items }, on_search=function(q) }
function M.build_popup(args)
	local t = args.theme or {}
	local d = args.data or {}

	-- Bausteine erzeugen
	local header_api = Header.build(d.user, t)
	local columns = Columns.build(d.left_items, d.right_items, t)

	-- Footer: neue API nutzen (Widget + API)
	local footer_widget, footer_api = Footer.build({
		power_items = d.power_items,
		on_search = args.on_search,
		t = t,
	})

	-- Popup bauen (Placement: links am Rand, über der unteren Bar)
	local popup_api = Popup.build({
		header = header_api.widget,
		cols = columns.widget,
		footer = footer_widget,
		theme = t,
		placement = placement.above_bar({
			position = "bottom", -- oder "top"
			gap = 2,
			align = "left", -- linker Rand
		}),
	})

	-- Öffentliche API (Delegation)
	local api = {}

	-- Sichtbarkeit
	function api:show(opts)
		popup_api:show(opts)
	end

	function api:hide()
		popup_api:hide()
	end

	function api:toggle(opts)
		popup_api:toggle(opts)
	end

	-- Inhalte setzen
	function api:set_left(items)
		columns:set_left(items)
	end

	function api:set_right(items)
		columns:set_right(items)
	end

	function api:set_user(name, avatar_path, subtitle)
		header_api:set_user(name, avatar_path, subtitle)
	end

	-- Footer-API (z. B. Suche fokussieren)
	if footer_api and footer_api.focus_search then
		api.focus_search = footer_api.focus_search
	end

	return api
end

return M
