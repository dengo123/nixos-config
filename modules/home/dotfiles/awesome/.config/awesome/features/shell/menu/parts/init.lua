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
-- Dialog-Overrides aus Theme ableiten (Default-Look für Dialoge)
---------------------------------------------------------------------
local function dialog_overrides_from_theme(t)
	return {
		bg = t.dialog_bg or t.header_bg or t.bg,
		fg = t.dialog_fg or t.header_fg or t.fg,
		radius = t.dialog_radius or t.popup_radius or 8,
		border = t.dialog_border or t.popup_border_color,
		border_width = t.dialog_border_width or t.popup_border_width or 1,
		backdrop = t.dialog_backdrop or "#00000088",
		btn_bg = t.dialog_btn_bg,
		btn_fg = t.dialog_btn_fg,
	}
end

-- flaches Merge (für Tabellen gleicher Ebene)
local function merge(a, b)
	local out = {}
	for k, v in pairs(a or {}) do
		out[k] = v
	end
	for k, v in pairs(b or {}) do
		out[k] = v
	end
	return out
end

---------------------------------------------------------------------
-- args = {
--   theme,
--   data = { user, left_items, right_items, power_items },
--   on_search = function(query) ... end
-- }
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

	-- ZENTRALER DIALOG-PROVIDER + Default-Overrides
	api.dialogs = Dialogs
	api.dialog_overrides = dialog_overrides_from_theme(t)

	-- Dialog öffnen (Theme + embed korrekt an Dialog übergeben)
	function api:open_dialog(name, overrides)
		local D = self.dialogs
		if D and type(D[name]) == "function" then
			-- Options für den Dialog bauen:
			--  - Theme-Basis = aktuelles Menü-Theme
			--  - dialog_overrides als Theme-Overrides mergen
			--  - zusätzlich übergebene overrides oben drauf
			local opts = { theme = merge(t, self.dialog_overrides or {}), embed = true }
			for k, v in pairs(overrides or {}) do
				if k == "theme" and type(v) == "table" then
					opts.theme = merge(opts.theme, v)
				else
					opts[k] = v
				end
			end
			local widget = D[name](opts) -- Dialog-Builder liefert Widget (für Overlay)
			popup_api:show_dialog(widget)
			return widget
		end
		if gears and gears.debug and gears.debug.print_warning then
			gears.debug.print_warning(("Unknown dialog name: %s"):format(tostring(name)))
		end
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
