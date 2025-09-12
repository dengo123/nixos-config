-- ~/.config/awesome/features/shell/menu/core.lua
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local placement = require("features.shell.menu.shared.placement")

local M = {}

-- args = {
--   theme = table,
--   data  = { user, left_items, right_items, power_items },
--   on_search = function(q)
-- }
function M.build_popup(args)
	local t = args.theme or {}
	local d = args.data or {}

	-- Bausteine
	local header_api = Header.build(d.user, t)
	local columns = Columns.build(d.left_items, d.right_items, t)

	-- Footer (Widget + API)
	local footer_widget, footer_api = Footer.build({
		power_items = d.power_items,
		on_search = args.on_search,
		t = t,
	})

	-- Popup mit on_hide-Hook (beendet aktive Suche beim Kollaps)
	local popup_api = Popup.build({
		header = header_api.widget,
		cols = columns.widget,
		footer = footer_widget,
		theme = t,
		placement = placement.above_bar({
			position = "bottom",
			gap = 2,
			align = "left",
		}),
		on_hide = function()
			if footer_api and footer_api.cancel_search then
				footer_api.cancel_search()
			end
		end,
	})

	-- Öffentliche API
	local api = {}

	-- Sichtbarkeit
	function api:show(opts)
		popup_api:show(opts)
	end

	function api:hide()
		popup_api:hide() -- on_hide feuert innerhalb von popup.lua
	end

	function api:toggle(opts)
		popup_api:toggle(opts) -- on_hide feuert beim Schließen
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

	-- Footer-APIs durchreichen
	if footer_api and footer_api.focus_search then
		api.focus_search = footer_api.focus_search
	end
	if footer_api and footer_api.cancel_search then
		api.cancel_search = footer_api.cancel_search
	end
	if footer_api and footer_api.is_search_active then
		api.is_search_active = footer_api.is_search_active
	end

	return api
end

return M
