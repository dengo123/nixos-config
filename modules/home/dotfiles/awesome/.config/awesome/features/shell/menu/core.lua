-- features/shell/menu/core.lua
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local placement = require("features.shell.menu.shared.placement")
local beautiful = require("beautiful")

local M = {}

function M.build_popup(args)
	local t = args.theme or {}
	local d = args.data or {}

	local header_api = Header.build(d.user, t)
	local columns = Columns.build(d.left_items, d.right_items, t)
	local footer = Footer.build(d.power_items, t)

	local place = placement.with_cursor_x(placement.above_bar({
		position = "bottom", -- "top" wenn Bar oben
		gap = 2,
		bar_height = function(s)
			return (s.mywibar and s.mywibar.height) or beautiful.wibar_height or 32
		end,
		align = "left",
	}))

	local popup_api = Popup.build({
		header = header_api.widget,
		cols = columns.widget,
		footer = footer,
		theme = t,
		placement = place,
	})

	local api = {}
	function api:show(opts)
		popup_api:show(opts)
	end

	function api:hide()
		popup_api:hide()
	end

	function api:toggle(opts)
		popup_api:toggle(opts)
	end

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
