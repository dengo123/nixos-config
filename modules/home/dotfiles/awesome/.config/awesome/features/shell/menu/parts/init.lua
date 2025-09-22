-- ~/.config/awesome/features/shell/menu/parts/base.lua
local gears = require("gears")
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local ThemeMod_ok, ThemeMod = pcall(require, "features.shell.menu.parts.theme")

local M = {}

local function resolve_theme(theme)
	-- 1) Fertiges Table?
	if type(theme) == "table" then
		return theme
	end

	-- 2) Factory-Funktion?
	if type(theme) == "function" then
		local ok, ret = pcall(theme)
		if ok and type(ret) == "table" then
			return ret
		end
		warn("Theme factory lieferte kein Table – fallback auf ThemeMod.get()")
	end

	-- 3) Benanntes Preset-String? (z. B. "luna_xp")
	if type(theme) == "string" and ThemeMod_ok and type(ThemeMod[theme]) == "function" then
		return ThemeMod[theme]() -- Preset auflösen
	end

	-- 4) Modul-Default
	if ThemeMod_ok and type(ThemeMod.get) == "function" then
		return ThemeMod.get({})
	end
end

-- args = { theme, data = { user, left_items, right_items, power_items }, on_search }
function M.build_popup(args)
	args = args or {}
	local t = resolve_theme(args.theme)
	local d = args.data or {}

	-- Bausteine
	local header_api = Header.build(d.user, t)
	local columns = Columns.build(d.left_items, d.right_items, t)

	-- Footer (Widget + API)
	local footer_widget, footer_api = Footer.build({
		power_items = d.power_items,
		on_search = args.on_search,
		t = t, -- wichtig: dasselbe t
	})

	-- Popup mit on_hide-Hook (beendet aktive Suche beim Kollaps)
	local popup_api = Popup.build({
		header = header_api.widget,
		cols = columns.widget,
		footer = footer_widget,
		theme = t, -- wichtig: dasselbe t
		on_hide = function()
			if footer_api and (footer_api.cancel_search or footer_api.cancel) then
				pcall(footer_api.cancel_search or footer_api.cancel)
			end
		end,
	})

	-- Defensive Aliase: akzeptiere Footer- oder reine Search-API
	local focus_local = footer_api and (footer_api.focus_search or footer_api.focus_local) or nil
	local focus_web = footer_api and (footer_api.focus_search_web or footer_api.focus_web) or nil
	local cancel_search = footer_api and (footer_api.cancel_search or footer_api.cancel) or nil
	local is_active = footer_api and (footer_api.is_search_active or footer_api.is_active) or nil
	local is_collapsed = footer_api and (footer_api.is_search_collapsed or footer_api.is_collapsed) or nil
	local set_engine = footer_api and (footer_api.set_search_engine or footer_api.set_engine) or nil
	local set_browser = footer_api and (footer_api.set_search_browser or footer_api.set_browser) or nil

	-- Öffentliche API
	local api = {}

	-- Sichtbarkeit
	function api:show(opts)
		popup_api:show(opts)
	end

	function api:hide()
		popup_api:hide()
	end -- on_hide feuert in popup.lua

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

	-- Search-Orchestrierung
	if focus_local then
		function api:focus_search()
			popup_api:show()
			gears.timer.delayed_call(function()
				pcall(focus_local)
			end)
		end
	end
	if focus_web then
		function api:focus_search_web()
			popup_api:show()
			gears.timer.delayed_call(function()
				pcall(focus_web)
			end)
		end
	end
	if cancel_search then
		function api:cancel_search()
			pcall(cancel_search)
		end
	end
	if is_active then
		function api:is_search_active()
			return is_active()
		end
	end
	if is_collapsed then
		function api:is_search_collapsed()
			return is_collapsed()
		end
	end
	if set_engine then
		function api:set_search_engine(url_fmt)
			pcall(set_engine, url_fmt)
		end
	end
	if set_browser then
		function api:set_search_browser(bin)
			pcall(set_browser, bin)
		end
	end

	return api
end

return M
