-- ~/.config/awesome/features/shell/menu/parts/init.lua
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")

local Header = require("features.shell.menu.parts.header")
local Columns = require("features.shell.menu.parts.columns")
local Footer = require("features.shell.menu.parts.footer")
local Popup = require("features.shell.menu.parts.popup")
local Lib = require("features.shell.menu.lib")

-- unified theme lives in lib/theme
local ThemeMod_ok, ThemeMod = pcall(require, "features.shell.menu.lib.theme")

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

	-- 3) Preset-String
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
-- Menü-Popup aufbauen
---------------------------------------------------------------------
function M.build_popup(args)
	args = args or {}
	local t = resolve_theme(args.theme)
	t.unify_focus_hover = true -- Hover = Fokus-Optik vereinheitlichen
	local d = args.data or {}

	-------------------------------------------------------------------
	-- Header / Columns / Footer
	-------------------------------------------------------------------
	local header_api = Header.build(d.user, t)
	local columns = Columns.build(d.left_items, d.right_items, t)
	local footer_widget, footer_api = Footer.build({
		power_items = d.power_items,
		on_search = args.on_search,
		t = t,
	})

	-------------------------------------------------------------------
	-- Höhe & feste Constraints
	-------------------------------------------------------------------
	local total_h = tonumber(t.total_height) or 650
	local header_h = tonumber(t.header_h) or 64
	local footer_h = tonumber(t.footer_h) or 48
	local body_h = math.max(1, total_h - header_h - footer_h)

	local header_fixed = wibox.widget({
		header_api.widget,
		strategy = "exact",
		height = header_h,
		widget = wibox.container.constraint,
	})
	local cols_fixed = wibox.widget({
		columns.widget,
		strategy = "exact",
		height = body_h,
		widget = wibox.container.constraint,
	})
	local footer_fixed = wibox.widget({
		footer_widget,
		strategy = "exact",
		height = footer_h,
		widget = wibox.container.constraint,
	})

	-------------------------------------------------------------------
	-- Abgerundeter Container um alles
	-------------------------------------------------------------------
	local rounded = wibox.widget({
		{
			header_fixed,
			cols_fixed,
			footer_fixed,
			layout = wibox.layout.fixed.vertical,
		},
		bg = t.popup_bg or t.dialog_bg or "#235CDB",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, tonumber(t.popup_radius) or 12)
		end,
		shape_clip = true,
		shape_border_color = t.popup_border_color or t.header_bg or "#3A6EA5",
		widget = wibox.container.background,
	})

	-------------------------------------------------------------------
	-- Popup-Lifecycle darum wickeln (mit echter Zentrierung)
	-------------------------------------------------------------------
	local popup_api = Popup.wrap(rounded, {
		theme = t,
		enable_esc_grabber = true,
		placement = function(p, s)
			-- Primary: wie vorher unten-links an die Workarea andocken
			if s == screen.primary then
				local wa = s.workarea or s.geometry
				local gap = 2
				local ph = (p.height and p.height > 0) and p.height or (t.total_height or 520)
				p.x = wa.x
				p.y = wa.y + wa.height - gap - ph
				return
			end
			-- Nicht-Primary: wirklich zentrieren
			awful.placement.centered(p, { parent = s, honor_workarea = true })
		end,
		on_hide = function()
			if footer_api and (footer_api.cancel_search or footer_api.cancel) then
				pcall(footer_api.cancel_search or footer_api.cancel)
			end
		end,
	})

	popup_api:set_window_chrome({
		radius = tonumber(t.popup_radius) or 12,
		border_color = t.popup_border_color or t.header_bg or "#3A6EA5",
	})

	-------------------------------------------------------------------
	-- Öffentliche Menü-API
	-------------------------------------------------------------------
	local api = {}
	local stop_focus = nil

	local function attach_menu_focus()
		local cols_focus = (columns.get_focus_items and columns:get_focus_items()) or { left = {}, right = {} }
		local power_focus = {}
		if footer_api and footer_api.get_power_focus_items then
			power_focus = footer_api.get_power_focus_items() or {}
		end

		local linear = {}
		for _, w in ipairs(cols_focus.left or {}) do
			table.insert(linear, w)
		end
		for _, w in ipairs(cols_focus.right or {}) do
			table.insert(linear, w)
		end
		for _, w in ipairs(power_focus or {}) do
			table.insert(linear, w)
		end

		if Lib and Lib.focus and type(Lib.focus.attach) == "function" then
			return Lib.focus.attach(linear, t, { handle = popup_api }) -- mouse_follow default=an
		end
		return nil
	end

	function api:show(opts)
		popup_api:show(opts)
		if stop_focus then
			pcall(stop_focus)
			stop_focus = nil
		end
		stop_focus = attach_menu_focus()
	end

	function api:hide()
		if stop_focus then
			pcall(stop_focus)
			stop_focus = nil
		end
		popup_api:hide()
	end

	function api:toggle(opts)
		if popup_api.is_visible and popup_api:is_visible() then
			api:hide()
		else
			api:show(opts)
		end
	end

	function api:set_left(items)
		columns:set_left(items)
	end

	function api:set_right(items)
		columns:set_right(items)
	end

	function api:set_user(name, avatar, sub)
		header_api:set_user(name, avatar, sub)
	end

	function api:show_dialog(w)
		if stop_focus then
			pcall(stop_focus)
			stop_focus = nil
		end
		popup_api:show_dialog(w)
	end

	function api:hide_dialog()
		popup_api:hide_dialog()
		if not stop_focus then
			stop_focus = attach_menu_focus()
		end
	end

	function api:get_theme()
		return t
	end

	-- Search-Bridge vom Footer
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

	-- Dispatcher andocken
	if Lib and type(Lib.init) == "function" then
		Lib.init(api, { flatten_helpers = false, flatten_focus = false })
	end

	function api:make_launcher(icon, beautiful_mod)
		return Popup.make_launcher(self, icon, beautiful_mod)
	end

	return api
end

return M
