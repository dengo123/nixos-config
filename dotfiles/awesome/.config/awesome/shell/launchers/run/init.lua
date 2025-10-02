-- shell/menu/search/init.lua
-- Orchestrator: Theme.resolve + Providers + View + Controller

local awful = require("awful")
local gears = require("gears")
local Theme = require("shell.menu.search.theme")
local Providers = require("shell.menu.search.providers")
local View = require("shell.menu.search.view")
local Controller = require("shell.menu.search.controller")

local M = {}

-- Menü schließen (API oder Signal)
local function hide_menu_popup()
	local api = rawget(_G, "__menu_api")
	if api and type(api.hide) == "function" then
		api.hide()
	else
		awesome.emit_signal("menu::hide")
	end
end

function M.new(opts)
	opts = opts or {}

	-- Theme laden
	local T = Theme.resolve(opts)

	local H = T.sizes.height
	local WIDTH_EXP = T.sizes.width_expanded
	local WIDTH_COL = T.sizes.width_collapsed

	local BG_ACTIVE = T.colors.bg_active
	local FG_ACTIVE = T.colors.fg_active
	local BG_COLLAPSED = T.colors.bg_collapsed
	local CURSOR_BG = T.colors.cursor_bg
	local CURSOR_FG = T.colors.cursor_fg

	local PAD_L, PAD_R, PAD_T, PAD_B = T.layout.left, T.layout.right, T.layout.top, T.layout.bottom
	local SPACING = T.layout.spacing

	local BROWSER = T.web.browser
	local ENGINE_FMT = T.web.engine

	local PREFIX_LOCAL = (T.prefix and T.prefix.local_mode) or "/"
	local PREFIX_WEB = (T.prefix and T.prefix.web_mode) or "?"

	local HOME = os.getenv("HOME") or "~"

	-- Prompt + Textbox (kommt aus awful.widget.prompt)
	local prompt = awful.widget.prompt({})
	local textbox = prompt.widget
	if textbox.set_align then
		textbox:set_align("left")
	end
	if textbox.set_valign then
		textbox:set_valign("center")
	end
	if textbox.set_text then
		textbox:set_text("")
	end
	textbox.bg = BG_ACTIVE
	textbox.fg = FG_ACTIVE
	textbox.bg_cursor = CURSOR_BG
	textbox.fg_cursor = CURSOR_FG

	-- View bauen
	local view = View.build({
		height = H,
		width_expanded = WIDTH_EXP,
		bg_active = BG_ACTIVE,
		fg_active = FG_ACTIVE,
		padding = { left = PAD_L, right = PAD_R, top = PAD_T, bottom = PAD_B },
		spacing = SPACING,
	}, textbox)

	local widget = view.widget
	local prefix_lbl = view.parts.prefix_lbl
	local inner_margin = view.parts.inner_margin
	local bg_box = view.parts.bg_box
	local width_ctl = view.parts.width_ctl

	-- Controller instanziieren
	local ctrl = Controller.new({
		parts = {
			textbox = textbox,
			prefix_lbl = prefix_lbl,
			inner_margin = inner_margin,
			bg_box = bg_box,
			width_ctl = width_ctl,
		},
		sizes = { width_expanded = WIDTH_EXP, width_collapsed = WIDTH_COL },
		colors = {
			bg_active = BG_ACTIVE,
			bg_collapsed = BG_COLLAPSED,
			fg_active = FG_ACTIVE,
			cursor_bg = CURSOR_BG,
			cursor_fg = CURSOR_FG,
		},
		layout = { left = PAD_L, right = PAD_R, top = PAD_T, bottom = PAD_B },
		prefixes = { local_mode = PREFIX_LOCAL, web_mode = PREFIX_WEB },
		providers = Providers,
		web = { browser = BROWSER, engine = ENGINE_FMT },
		home = HOME,
		awful = awful,
		gears = gears,
		hide_menu_popup = hide_menu_popup,
	})

	-- Mausaktionen binden & initialer Zustand
	ctrl.bind_mouse(widget)
	ctrl.init()

	-- Public API (einfach Controller durchreichen)
	local api = {
		focus_local = ctrl.focus_local,
		focus_web = ctrl.focus_web,
		cancel = ctrl.cancel,
		is_active = ctrl.is_active,
		is_collapsed = ctrl.is_collapsed,
		set_engine = ctrl.set_engine,
		set_browser = ctrl.set_browser,
	}

	return { widget = widget, api = api }
end

return M
