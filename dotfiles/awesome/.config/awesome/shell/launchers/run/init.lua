-- ~/.config/awesome/shell/launchers/run/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Container = require("shell.launchers.run.container")
local View = require("shell.launchers.run.view")
local Providers = require("shell.launchers.run.providers")
local Controller = require("shell.launchers.run.controller")

local M = {}

local function must(cond, msg)
	assert(cond, "run/init.lua: " .. msg)
end

local function dims(panel)
	local H = assert(tonumber(panel.height), "panel.height missing/invalid")
	local header = assert(tonumber(panel.header_h), "panel.header_h missing/invalid")
	local footer = assert(tonumber(panel.footer_h), "panel.footer_h missing/invalid")
	local pad_h = assert(tonumber(panel.pad_h), "panel.pad_h missing/invalid")
	local pad_v = assert(tonumber(panel.pad_v), "panel.pad_v missing/invalid")
	return {
		w = assert(tonumber(panel.width), "panel.width missing/invalid"),
		h = H,
		header_h = header,
		footer_h = footer,
		body_h = math.max(0, H - header - footer),
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

function M.open(opts, Lib)
	opts = opts or {}
	Lib = Lib or require("shell.launchers.lib")

	-- Theme via injizierter UI-API
	must(
		Lib and Lib.ui_api and type(Lib.ui_api.resolve_theme) == "function",
		"Lib.ui_api.resolve_theme not available (injection required)"
	)
	local th = Lib.ui_api.resolve_theme("run", opts.theme or {})
	must(th and th.panel and th.search, "theme for 'run' must provide {panel, search}")

	local d = dims(th.panel)

	-- Prompt/Textbox
	local prompt = awful.widget.prompt()
	local textbox = prompt.widget

	-- View (Searchbar + Parts)
	local ui = {
		body_width = (th.panel.width - 2 * d.pad_h),
		height = th.search.sizes.height,

		bg_active = th.search.colors.bg_active,
		fg_active = th.search.colors.fg_active,

		padding = {
			left = th.search.layout.left,
			right = th.search.layout.right,
			top = th.search.layout.top,
			bottom = th.search.layout.bottom,
		},
		spacing = th.search.layout.spacing,
		border_w = th.search.border_w,
		border_color = th.search.border_color,

		label_open_text = th.search.label_open_text,
		label_open_width = th.search.label_open_width,
		hint = th.search.hint,
	}
	local view = View.build(ui, textbox)

	-- ===== Searchbar exakt in die MITTE DES BODY setzen (Footer ignorieren) =====
	local bar_h = th.search.sizes.height

	-- Höhe der Hint-Zeile (falls aktiv) grob bestimmen:
	local hint = th.search.hint or {}
	local hint_enabled = (hint.show ~= false) and (hint.text and #tostring(hint.text) > 0)
	local pad_t = tonumber(th.search.layout.top) or 6
	local spacing = tonumber(hint.spacing) or 6
	local hint_sz = tonumber(hint.size) or 12
	-- view.lua setzt für die Hint-Zeile: top = pad_t, bottom = 0 und darunter spacing
	local hint_h = hint_enabled and (pad_t + hint_sz + spacing) or 0

	-- T so wählen, dass: T + hint_h + bar_h/2 = d.body_h/2
	local offset_in_body = math.max(0, math.floor(d.body_h / 2 - (hint_h + bar_h / 2)))

	local body_widget = wibox.widget({
		view.widget,
		top = offset_in_body,
		widget = wibox.container.margin,
	})

	-- Buttons (Look-only), Aktionen werden nach Controller gebunden
	local Button = (Lib and Lib.button) or require("shell.launchers.lib.button")
	local act_ok = function() end
	local act_cancel = function() end

	local ok_btn = Button.mk_button("OK", function()
		act_ok()
	end)
	local cancel_btn = Button.mk_button("Cancel", function()
		act_cancel()
	end)

	-- Container (Header/Body/Footer)
	local stack = Container.build(th.panel, d, {
		title = th.panel.title or opts.title or "Run",
		body = body_widget,
		ok_btn = ok_btn,
		cancel_btn = cancel_btn,
	})

	-- Popup öffnen: ohne Backdrop/Root, ABER mit Fenster-Shape (runde Ecken!)
	must(type(Lib.ui_api.open_panel) == "function", "Lib.ui_api.open_panel missing")
	local handle = Lib.ui_api.open_panel(stack, th.panel, {
		use_backdrop = false,
		show_root = false,
		screen = opts.screen or (mouse and mouse.screen) or awful.screen.focused(),
		shape = function(cr, w, h)
			local r = tonumber(th.panel.panel_radius) or tonumber(th.panel_radius) or 12
			gears.shape.rounded_rect(cr, w, h, r)
		end,
	})

	-- Controller (Prompt lifecycle + Provider-Dispatch)
	local ctrl = Controller.new({
		awful = awful,
		gears = gears,
		parts = {
			textbox = textbox,
			prefix_lbl = view.parts.prefix_lbl,
			inner_margin = view.parts.inner_margin,
			bg_box = view.parts.field_bg,
			width_ctl = view.parts.width_ctl,
		},
		sizes = { width_expanded = th.search.sizes.width_expanded },
		colors = {
			bg_active = th.search.colors.bg_active,
			fg_active = th.search.colors.fg_active,
			cursor_bg = th.search.colors.cursor_bg,
			cursor_fg = th.search.colors.cursor_fg,
		},
		layout = {
			left = th.search.layout.left,
			right = th.search.layout.right,
			top = th.search.layout.top,
			bottom = th.search.layout.bottom,
		},
		prefixes = th.search.prefix,
		providers = Providers,
		web = th.search.web,
		home = os.getenv("HOME"),
		hide_menu_popup = function()
			if handle and handle.close then
				handle.close()
			end
		end,
	})
	ctrl.init()

	-- ✅ API global bereitstellen, damit die Hotkeys umschalten können
	rawset(_G, "__run_api", ctrl)

	-- ✅ Gewünschten Startmodus respektieren
	local start_mode = (opts.mode == "local" or opts.mode == "files") and "local"
		or (opts.mode == "web" and "web")
		or "run"

	if start_mode == "local" and ctrl.focus_local then
		ctrl.focus_local()
	elseif start_mode == "web" and ctrl.focus_web then
		ctrl.focus_web()
	else
		ctrl.focus_run()
	end

	-- Aktionen binden: Labels -> Funktionen
	local Actions = (Lib and Lib.actions) or require("shell.launchers.lib.actions")
	local bound = Actions.bind({ ctrl = ctrl, handle = handle, gears = gears })
	act_ok = bound["OK"] or act_ok
	act_cancel = bound["Cancel"] or act_cancel

	-- Beim Schließen die globale API wieder freigeben
	local function cleanup_api()
		if rawget(_G, "__run_api") == ctrl then
			rawset(_G, "__run_api", nil)
		end
	end

	if handle and type(handle.on_close) == "function" then
		handle.on_close(cleanup_api)
	end
	if handle and type(handle.close) == "function" then
		local old_close = handle.close
		handle.close = function(...)
			cleanup_api()
			return old_close(...)
		end
	end

	return handle
end

return M
