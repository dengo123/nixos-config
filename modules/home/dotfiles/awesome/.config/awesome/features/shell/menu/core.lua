-- features/shell/menu/core.lua
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local placement = require("features.shell.menu.shared.placement")

local M = {}

-- args = { theme=table, data={ user, left_items, right_items, power_items } }
function M.build_popup(args)
	local t = args.theme or {}
	local d = args.data or {}

	-- Bausteine erzeugen
	local header_api = Header.build(d.user, t)
	local columns = Columns.build(d.left_items, d.right_items, t)
	local footer = Footer.build(d.power_items, t)

	-- Popup bauen (Placement: links am Rand, über der unteren Bar)
	local popup_api = Popup.build({
		header = header_api.widget,
		cols = columns.widget,
		footer = footer,
		theme = t,
		placement = placement.above_bar({
			position = "bottom", -- "top", falls deine Bar oben sitzt
			gap = 2,
			align = "left", -- am linken Workarea-Rand
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

	-- Inhalte
	function api:set_left(items)
		columns:set_left(items)
	end

	function api:set_right(items)
		columns:set_right(items)
	end

	function api:set_user(name, avatar_path, subtitle)
		header_api:set_user(name, avatar_path, subtitle)
	end

	return api
end

return M
