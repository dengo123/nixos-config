-- ~/.config/awesome/shell/launchers/run/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Container = require("shell.launchers.run.container")
local View = require("shell.launchers.run.view")
local Providers = require("shell.launchers.run.providers")
local Complete = require("shell.launchers.run.complete")
local Controller = require("shell.launchers.run.controller")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function must(cond, msg)
	assert(cond, "run/init.lua: " .. msg)
end

local function dims(panel)
	local h = assert(tonumber(panel.height), "panel.height missing/invalid")
	local header_h = assert(tonumber(panel.header_h), "panel.header_h missing/invalid")
	local footer_h = assert(tonumber(panel.footer_h), "panel.footer_h missing/invalid")
	local pad_h = assert(tonumber(panel.pad_h), "panel.pad_h missing/invalid")
	local pad_v = assert(tonumber(panel.pad_v), "panel.pad_v missing/invalid")

	return {
		w = assert(tonumber(panel.width), "panel.width missing/invalid"),
		h = h,
		header_h = header_h,
		footer_h = footer_h,
		body_h = math.max(0, h - header_h - footer_h),
		pad_h = pad_h,
		pad_v = pad_v,
	}
end

local function resolve_theme(Lib, overrides)
	must(Lib and Lib.ui_api and type(Lib.ui_api.resolve_theme) == "function", "Lib.ui_api.resolve_theme not available")
	return Lib.ui_api.resolve_theme("run", overrides or {})
end

local function resolve_web_cfg(cfg)
	local system_cfg = cfg.system or {}
	local search_cfg = cfg.search or {}

	return search_cfg.web
		or {
			browser = system_cfg.browser or "firefox",
			engine = "https://duckduckgo.com/?q=%s",
		}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.open(opts, Lib)
	opts = opts or {}
	Lib = Lib or require("shell.launchers")

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local cfg = opts.cfg or {}
	local web_cfg = resolve_web_cfg(cfg)

	local th = resolve_theme(Lib, opts.theme)
	must(th and th.panel and th.search, "theme for 'run' must provide {panel, search}")

	local panel = th.panel
	local search = th.search
	local buttons = assert(th.buttons, "theme for 'run' must provide buttons")
	local d = dims(panel)

	-- ---------------------------------------------------------------------
	-- Prompt
	-- ---------------------------------------------------------------------

	local prompt = awful.widget.prompt()
	local textbox = prompt.widget

	local ui = {
		body_width = panel.width - 2 * d.pad_h,
		height = assert(tonumber(search.sizes.height), "theme.run.search.sizes.height missing/invalid"),

		bg_active = search.colors.bg_active,
		fg_active = search.colors.fg_active,

		padding = {
			left = search.layout.left,
			right = search.layout.right,
			top = search.layout.top,
			bottom = search.layout.bottom,
		},

		spacing = search.layout.spacing,
		border_w = search.border_w,
		border_color = search.border_color,

		prefix_width = search.prefix_width,
		prefix_font = search.prefix_font,
		prefix_size = search.prefix_size,
		hint = search.hint,
	}

	local view = View.build(ui, textbox)

	-- ---------------------------------------------------------------------
	-- Body
	-- ---------------------------------------------------------------------

	local bar_h = assert(tonumber(search.sizes.height), "theme.run.search.sizes.height missing/invalid")
	local hint = search.hint or {}
	local hint_enabled = (hint.show ~= false) and (hint.text and #tostring(hint.text) > 0)

	local pad_t = assert(tonumber(search.layout.top), "theme.run.search.layout.top missing/invalid")
	local hint_spacing = assert(tonumber(hint.spacing), "theme.run.search.hint.spacing missing/invalid")
	local hint_size = assert(tonumber(hint.size), "theme.run.search.hint.size missing/invalid")

	local hint_h = hint_enabled and (pad_t + hint_size + hint_spacing) or 0
	local offset_in_body = math.max(0, math.floor(d.body_h / 2 - (hint_h + bar_h / 2)))

	local body_widget = wibox.widget({
		view.widget,
		top = offset_in_body,
		widget = wibox.container.margin,
	})

	-- ---------------------------------------------------------------------
	-- Footer Buttons
	-- ---------------------------------------------------------------------

	local Button = assert(Lib and Lib.button, "run: Lib.button fehlt")

	local act_mode = function() end
	local act_ok = function() end
	local act_cancel = function() end

	local mode_btn = Button.mk_button(buttons.mode, function()
		act_mode()
	end)

	local ok_btn = Button.mk_button(buttons.ok, function()
		act_ok()
	end)

	local cancel_btn = Button.mk_button(buttons.cancel, function()
		act_cancel()
	end)

	-- ---------------------------------------------------------------------
	-- Container
	-- ---------------------------------------------------------------------

	local stack = Container.build(panel, d, {
		title = opts.title or panel.title,
		body = body_widget,
		footer_buttons = {
			mode_btn,
			ok_btn,
			cancel_btn,
		},
	})

	must(type(Lib.ui_api.open_panel) == "function", "Lib.ui_api.open_panel missing")

	local handle = Lib.ui_api.open_panel(stack, panel, {
		use_backdrop = false,
		show_root = false,
		screen = opts.screen or (mouse and mouse.screen) or awful.screen.focused(),
		shape = function(cr, w, h)
			local r = assert(tonumber(panel.panel_radius), "theme.run.panel.panel_radius missing/invalid")
			gears.shape.rounded_rect(cr, w, h, r)
		end,
	})

	-- ---------------------------------------------------------------------
	-- Controller
	-- ---------------------------------------------------------------------

	local ctrl = Controller.new({
		awful = awful,
		gears = gears,
		view = view,

		parts = {
			textbox = textbox,
		},

		sizes = {
			width_expanded = search.sizes.width_expanded,
		},

		colors = {
			bg_active = search.colors.bg_active,
			fg_active = search.colors.fg_active,
			cursor_bg = search.colors.cursor_bg,
			cursor_fg = search.colors.cursor_fg,
		},

		layout = {
			left = search.layout.left,
			right = search.layout.right,
			top = search.layout.top,
			bottom = search.layout.bottom,
		},

		prefixes = search.prefix,
		providers = Providers,
		complete = Complete,
		web = web_cfg,
		home = os.getenv("HOME"),

		hide_menu_popup = function()
			if handle and handle.close then
				handle.close()
			end
		end,
	})

	ctrl.init()

	-- ---------------------------------------------------------------------
	-- Global API
	-- ---------------------------------------------------------------------

	rawset(_G, "__run_api", ctrl)

	local start_mode = (opts.mode == "local" or opts.mode == "files") and "local"
		or (opts.mode == "web" and "web")
		or "run"

	if start_mode == "local" then
		ctrl.focus_local()
	elseif start_mode == "web" then
		ctrl.focus_web()
	else
		ctrl.focus_run()
	end

	-- ---------------------------------------------------------------------
	-- Bind Actions
	-- ---------------------------------------------------------------------

	local Actions = assert(Lib and Lib.actions, "run: Lib.actions fehlt")
	local bound = Actions.bind({
		ctrl = ctrl,
		handle = handle,
		gears = gears,
	})

	act_mode = bound[buttons.mode] or act_mode
	act_ok = bound[buttons.ok] or act_ok
	act_cancel = bound[buttons.cancel] or act_cancel

	-- ---------------------------------------------------------------------
	-- Cleanup
	-- ---------------------------------------------------------------------

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
