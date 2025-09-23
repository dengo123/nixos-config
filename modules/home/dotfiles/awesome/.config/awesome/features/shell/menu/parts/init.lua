-- ~/.config/awesome/features/shell/menu/parts/init.lua
local gears = require("gears")
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local Dialogs = require("features.shell.menu.dialogs")
local Actions = require("features.shell.menu.lib.actions")

local ThemeMod_ok, ThemeMod = pcall(require, "features.shell.menu.parts.theme")

local M = {}

---------------------------------------------------------------------
-- Theme-Resolver
---------------------------------------------------------------------
local function resolve_theme(theme)
	-- 1) Fertiges Table
	if type(theme) == "table" then
		return theme
	end

	-- 2) Factory-Funktion
	if type(theme) == "function" then
		local ok, ret = pcall(theme)
		if ok and type(ret) == "table" then
			return ret
		end
		if gears and gears.debug and gears.debug.print_warning then
			gears.debug.print_warning("Theme factory lieferte kein Table – fallback auf ThemeMod.get()")
		end
	end

	-- 3) Preset-String (z. B. "luna_xp")
	if type(theme) == "string" and ThemeMod_ok and type(ThemeMod[theme]) == "function" then
		return ThemeMod[theme]()
	end

	-- 4) Modul-Default
	if ThemeMod_ok and type(ThemeMod.get) == "function" then
		return ThemeMod.get({})
	end

	-- 5) Fallback
	return {}
end

---------------------------------------------------------------------
function M.build_popup(args)
	args = args or {}
	local t = resolve_theme(args.theme)
	local d = args.data or {}

	-------------------------------------------------------------------
	-- Header
	-------------------------------------------------------------------
	local header_api = Header.build(d.user, t)

	-------------------------------------------------------------------
	-- Columns
	-------------------------------------------------------------------
	local columns = Columns.build(d.left_items, d.right_items, t)

	-------------------------------------------------------------------
	-- Footer (Widget + API; kapselt Suche/Bindings)
	-------------------------------------------------------------------
	local footer_widget, footer_api = Footer.build({
		power_items = d.power_items,
		on_search = args.on_search,
		t = t,
	})

	-------------------------------------------------------------------
	-- Menü-Popup (Hülle um Header/Columns/Footer)
	-------------------------------------------------------------------
	local popup_api = Popup.build({
		header = header_api.widget,
		cols = columns.widget,
		footer = footer_widget,
		theme = t,
		on_hide = function()
			-- Suche schließen, wenn das Menü kollabiert
			if footer_api and (footer_api.cancel_search or footer_api.cancel) then
				pcall(footer_api.cancel_search or footer_api.cancel)
			end
		end,
	})

	-------------------------------------------------------------------
	-- Öffentliche Menü-API
	-------------------------------------------------------------------
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

	function api:set_user(name, avatar, sub)
		header_api:set_user(name, avatar, sub)
	end

	-- Dialog-Brücke / Theme-Zugriff
	function api:show_dialog(w)
		popup_api:show_dialog(w)
	end

	function api:hide_dialog()
		popup_api:hide_dialog()
	end

	function api:get_theme()
		return t
	end

	-- Search-Orchestrierung (vom Footer-API durchgereicht)
	if footer_api then
		if footer_api.focus_search then
			function api:focus_search()
				popup_api:show()
				gears.timer.delayed_call(function()
					pcall(footer_api.focus_search)
				end)
			end
		end
		if footer_api.focus_search_web then
			function api:focus_search_web()
				popup_api:show()
				gears.timer.delayed_call(function()
					pcall(footer_api.focus_search_web)
				end)
			end
		end
		if footer_api.cancel_search then
			function api:cancel_search()
				pcall(footer_api.cancel_search)
			end
		end
		if footer_api.is_search_active then
			function api:is_search_active()
				return footer_api.is_search_active()
			end
		end
		if footer_api.is_search_collapsed then
			function api:is_search_collapsed()
				return footer_api.is_search_collapsed()
			end
		end
		if footer_api.set_search_engine then
			function api:set_search_engine(url_fmt)
				pcall(footer_api.set_search_engine, url_fmt)
			end
		end
		if footer_api.set_search_browser then
			function api:set_search_browser(bin)
				pcall(footer_api.set_search_browser, bin)
			end
		end
	end

	-- Dispatcher an die API andocken (zentral!)
	if Actions and type(Actions.init) == "function" then
		Actions.init(api)
	end

	return api
end

return M
