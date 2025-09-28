-- ~/.config/awesome/features/shell/menu/container/init.lua
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")

local Header = require("features.shell.menu.container.header")
local Columns = require("features.shell.menu.layouts.columns")
local Footer = require("features.shell.menu.container.footer")
local Frame = require("features.shell.menu.container.frame")
local Popup = require("features.shell.menu.container.popup")
local Lib = require("features.shell.menu.lib")

-- zentrales Theme-Modul (Defaults, row_colors, etc.)
local Theme_ok, Theme = pcall(require, "features.shell.menu.lib.theme")

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
			gears.debug.print_warning("Theme factory lieferte kein Table – fallback auf Theme.get()")
		end
	end

	-- 3) Preset-String
	if type(theme) == "string" and Theme_ok and type(Theme[theme]) == "function" then
		return Theme[theme]()
	end

	-- 4) Modul-Default
	if Theme_ok and type(Theme.get) == "function" then
		return Theme.get({})
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
	t.unify_focus_hover = true
	local d = args.data or {}

	-------------------------------------------------------------------
	-- Header / Columns / Footer
	-------------------------------------------------------------------
	local header_api = Header.build(d.user, t)

	-- Spalten-Farbpaletten und Zeilenhöhen aus Theme ableiten
	local left_colors = Theme_ok and Theme.row_colors(t, "left") or { bg = t.left_bg or "#FFFFFF" }
	local right_colors = Theme_ok and Theme.row_colors(t, "right") or { bg = t.right_bg or "#D2E5FA" }

	local columns = Columns.build(d.left_items, d.right_items, t, {
		-- Spaltenbreiten/Abstände
		left_w = tonumber(t.col_left_w) or 250,
		right_w = tonumber(t.col_right_w) or 230,
		spacing = tonumber(t.col_spacing) or 1,
		pad_l = tonumber(t.cols_pad_l) or 2,
		pad_r = tonumber(t.cols_pad_r) or 2,
		pad_t = tonumber(t.cols_pad_t) or 2,
		pad_b = tonumber(t.cols_pad_b) or 2,

		-- durchgehende Spaltenhintergründe (links/rechts)
		left_bg = t.left_bg or left_colors.bg,
		right_bg = t.right_bg or right_colors.bg,

		-- Row-Styles/-Höhen werden an die Rows (P.list_widget) durchgereicht
		left_opts = {
			colors = left_colors, -- { bg, fg, hover }
			row_h = tonumber(t.left_row_h or t.row_h), -- eigene Höhe links
		},
		right_opts = {
			colors = right_colors,
			row_h = tonumber(t.right_row_h or t.row_h), -- eigene Höhe rechts
		},
	})

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

	local header_fixed = Frame.fixed_height(header_api.widget, header_h)
	local cols_fixed = Frame.fixed_height(columns.widget, body_h)
	local footer_fixed = Frame.fixed_height(footer_widget, footer_h)

	-------------------------------------------------------------------
	-- Chrome/Framing zentral via Frame
	-------------------------------------------------------------------
	local framed = Frame.wrap(
		{
			header = header_fixed,
			body = cols_fixed,
			footer = footer_fixed,
		},
		t,
		{
			total_height = total_h,
			radius = tonumber(t.popup_radius) or 12,
			border_width = tonumber(t.popup_border_width) or 1,
			border_color = t.popup_border_color or t.header_bg or "#3A6EA5",
			inner_bg = t.dialog_bg or "#235CDB",
			outer_bg = t.popup_bg, -- optional (z. B. transparent)
		}
	)

	-------------------------------------------------------------------
	-- Popup-Lifecycle (ESC, Outside-Click, Dialog-Overlay)
	-------------------------------------------------------------------
	local popup_api = Popup.wrap(framed, {
		theme = t,
		enable_esc_grabber = true,
		placement = function(p, s)
			-- Primary: unten links andocken, andere Screens: zentrieren
			if s == screen.primary then
				local wa = s.workarea or s.geometry
				local gap = 2
				local ph = (p.height and p.height > 0) and p.height or (t.total_height or 520)
				p.x = wa.x
				p.y = wa.y + wa.height - gap - ph
				return
			end
			awful.placement.centered(p, { parent = s, honor_workarea = true })
		end,
		on_hide = function()
			if footer_api and (footer_api.cancel_search or footer_api.cancel) then
				pcall(footer_api.cancel_search or footer_api.cancel)
			end
		end,
	})

	-------------------------------------------------------------------
	-- Öffentliche Menü-API
	-------------------------------------------------------------------
	local api = {}
	local stop_focus = nil

	local function attach_menu_focus()
		-- 1) Fokus-Items der Spalten
		local cols_focus = (columns.get_focus_items and columns:get_focus_items()) or { left = {}, right = {} }

		-- 2) Power-Fokus aus Footer
		local power_focus = {}
		if footer_api and footer_api.get_power_focus_items then
			power_focus = footer_api.get_power_focus_items() or {}
		end

		-- 3) Orchestrator: Links/Rechts + Power kombinieren
		if Lib and Lib.focus and type(Lib.focus.attach_columns_power) == "function" then
			return Lib.focus.attach_columns_power(cols_focus.left or {}, cols_focus.right or {}, power_focus or {}, t, {
				handle = popup_api, -- für ESC etc.
				mouse_follow = true, -- Hover aktualisiert Tastaturfokus
				start_side = "left", -- initial links
			})
		end

		-- Fallback: linear
		if Lib and Lib.focus and type(Lib.focus.attach) == "function" then
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
			return Lib.focus.attach(linear, t, { handle = popup_api })
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
