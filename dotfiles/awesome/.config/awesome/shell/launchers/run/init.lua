-- ~/.config/awesome/shell/launchers/run/init.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local Container = require("shell.launchers.run.container")
local View = require("shell.launchers.run.view")
local Ctrlf = require("shell.launchers.run.controller")
local Providers = require("shell.launchers.run.providers")

local M = {}

local function center(child)
	return wibox.widget({
		child,
		halign = "center",
		valign = "center",
		widget = wibox.container.place,
	})
end

-- Theme über opts.theme ODER über eine zentrale UI-API auflösen
local function resolve_theme(opts, Lib)
	if opts and opts.theme then
		return opts.theme
	end
	if Lib and Lib.ui_api and Lib.ui_api.resolve_theme then
		return Lib.ui_api.resolve_theme("run", opts and opts.theme_overrides or nil)
	end
	error("run.init: kein Theme verfügbar (erwarte opts.theme ODER Lib.ui_api.resolve_theme('run'))")
end

-- Screen zentral aus Lib.ui_api, sonst Maus/Fokus
local function resolve_screen(opts, Lib)
	if Lib and Lib.ui_api and Lib.ui_api.resolve_screen then
		return Lib.ui_api.resolve_screen(opts)
	end
	return (opts and opts.screen) or (mouse and mouse.screen) or awful.screen.focused()
end

function M.open(opts, injected_lib)
	opts = opts or {}

	-- Lib wird wie bei power/ injiziert; KEIN Fallback auf andere Popup-Requires!
	local Lib = injected_lib or require("shell.launchers.lib")

	-- Popup MUSS aus Lib kommen (gleiche Infrastruktur wie power/)
	assert(
		Lib and Lib.popup and Lib.popup.show,
		"run.init: Lib.popup.show fehlt (launchers/init muss L.lib injizieren)"
	)

	-- THEME + SCREEN
	local th = resolve_theme(opts, Lib)
	assert(th and th.panel and th.search, "run.init: Theme unvollständig (erwarte .panel und .search)")
	local s = resolve_screen(opts, Lib)

	local dims = {
		w = assert(tonumber(th.panel.width), "run.init: panel.width fehlt/ungültig"),
		h = assert(tonumber(th.panel.height), "run.init: panel.height fehlt/ungültig"),
		footer_h = tonumber(th.panel.footer_h) or 0,
	}

	-- VIEW
	local prompt = awful.widget.prompt()
	local view = View.build({
		height = th.search.sizes.height,
		width_expanded = th.search.sizes.width_expanded,
		bg_active = th.search.colors.bg_active,
		fg_active = th.search.colors.fg_active,
		padding = {
			left = th.search.layout.left,
			right = th.search.layout.right,
			top = th.search.layout.top,
			bottom = th.search.layout.bottom,
		},
		spacing = th.search.layout.spacing,
	}, prompt.widget)

	-- CONTROLLER
	local popup_handle -- forward ref
	local ctrl = Ctrlf.new({
		parts = view.parts,
		sizes = th.search.sizes,
		colors = th.search.colors,
		layout = th.search.layout,
		prefixes = {
			run_mode = th.search.prefix.run_mode,
			local_mode = th.search.prefix.local_mode,
			web_mode = th.search.prefix.web_mode,
		},
		providers = Providers,
		web = th.search.web,
		home = os.getenv("HOME"),
		awful = awful,
		gears = gears,
		hide_menu_popup = function()
			if popup_handle and popup_handle.close then
				popup_handle.close()
			end
		end,
	})
	ctrl.init()

	-- CONTAINER
	local panel = Container.build({
		panel_radius = th.panel.radius,
		panel_border_width = th.panel.border_w,
		panel_border = th.panel.border,
		panel_header_h = th.panel.header_h,
		panel_header_bg = th.panel.header_bg,
		panel_header_fg = th.panel.header_fg,
		panel_body_bg = th.panel.body_bg,
		panel_body_fg = th.panel.body_fg,
		panel_bg = th.panel.bg,
		header_bg = th.panel.header_bg, -- Fallback im Container
	}, { w = dims.w, h = dims.h, footer_h = dims.footer_h }, {
		title = th.panel.title or "Run",
		body = center(view.widget),
		cancel_btn = {
			activate = function()
				ctrl.cancel()
				if popup_handle and popup_handle.close then
					popup_handle.close()
				end
			end,
		},
	})

	-- POPUP (AUSSCHLIESSLICH über injiziertes Lib)
	popup_handle = Lib.popup.show(panel, th.panel, {
		screen = s,
		width = dims.w,
		height = dims.h,
		placement = function(w)
			awful.placement.centered(w, { parent = s, honor_workarea = true })
		end,
		use_backdrop = true,
		show_root = "with_bars",
		group = "launchers",
	})

	-- API für Keybinds exponieren
	rawset(_G, "__run_api", {
		focus_run = function()
			ctrl.focus_run()
		end,
		focus_local = function()
			ctrl.focus_local()
		end,
		focus_web = function()
			ctrl.focus_web()
		end,
		is_active = function()
			return ctrl.is_active()
		end,
		cancel = function()
			ctrl.cancel()
			if popup_handle and popup_handle.close then
				popup_handle.close()
			end
		end,
	})

	-- Startmodus
	local start_mode = (opts.mode == "web") and "web" or (opts.mode == "files") and "local" or "run"
	if start_mode == "web" then
		ctrl.focus_web()
	elseif start_mode == "local" then
		ctrl.focus_local()
	else
		ctrl.focus_run()
	end

	return {
		close = function()
			ctrl.cancel()
			if popup_handle and popup_handle.close then
				popup_handle.close()
			end
		end,
	}
end

return M
