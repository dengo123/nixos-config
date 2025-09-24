-- ~/.config/awesome/features/shell/menu/parts/init.lua
local gears = require("gears")
local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local Lib = require("features.shell.menu.lib")

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

	-- Handle zum Stoppen des Menü-Fokus (Keygrabber/Cleanup)
	local stop_focus = nil

	-- Helfer: Menü-Fokus anhängen (Spalten-aware, Fallback linear)
	local function attach_menu_focus()
		-- Fokus-Items aus Spalten holen
		local cols_focus = (columns.get_focus_items and columns:get_focus_items()) or { left = {}, right = {} }

		if Lib and Lib.focus then
			if type(Lib.focus.attach_columns) == "function" then
				return Lib.focus.attach_columns(cols_focus, t, {
					handle = popup_api, -- für Esc/cleanup
					start_side = "left",
					wrap = true,
					-- keys = { left="Left", right="Right", up="Up", down="Down", ok={"Return","KP_Enter"}, cancel="Escape" },
				})
			elseif type(Lib.focus.attach) == "function" then
				local linear = {}
				for _, w in ipairs(cols_focus.left or {}) do
					table.insert(linear, w)
				end
				for _, w in ipairs(cols_focus.right or {}) do
					table.insert(linear, w)
				end
				return Lib.focus.attach(linear, t, { handle = popup_api })
			end
		end
		return nil
	end

	-- Sichtbarkeit
	function api:show(opts)
		popup_api:show(opts)

		-- alten Fokus-Grabber (falls vorhanden) stoppen
		if stop_focus then
			pcall(stop_focus)
			stop_focus = nil
		end

		-- Fokus neu anhängen
		stop_focus = attach_menu_focus()
	end

	function api:hide()
		-- zuerst Fokus abräumen
		if stop_focus then
			pcall(stop_focus)
			stop_focus = nil
		end
		popup_api:hide()
	end

	-- dot-safe toggle (funktioniert bei api.toggle(...) und api:toggle(...))
	function api:toggle(opts)
		if popup_api.is_visible and popup_api:is_visible() then
			api:hide()
		else
			api:show(opts)
		end
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
		-- Menü-Fokus pausieren, sonst frisst er die Keys des Dialogs
		if stop_focus then
			pcall(stop_focus)
			stop_focus = nil
		end
		popup_api:show_dialog(w)
	end

	function api:hide_dialog()
		popup_api:hide_dialog()
		-- Menü-Fokus nach dem Dialog wieder anhängen
		if not stop_focus then
			stop_focus = attach_menu_focus()
		end
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
	if Lib and type(Lib.init) == "function" then
		Lib.init(api, { flatten_helpers = false, flatten_focus = false })
	end

	-------------------------------------------------------------------
	-- Launcher-Builder an die API hängen (Option B)
	-------------------------------------------------------------------
	function api:make_launcher(icon, beautiful_mod)
		-- delegiert an parts.popup
		return Popup.make_launcher(self, icon, beautiful_mod)
	end

	return api
end

return M
