-- ~/.config/awesome/ui/theme/run.lua
-- Definiert Größe/Farben des Run-Launchers (Container + Suchleiste).
local colors = require("ui.colors")
local H = require("ui.helpers")

local M = {}

local function pick(a, b)
	return (a ~= nil) and a or b
end

function M.resolve(opts)
	opts = opts or {}
	local C = colors.get and colors.get() or colors

	-- Panel/Container
	local creme = C.creme or "#f7f2e7"
	local black = C.black or "#000"
	local border = (H.adjust_color and H.adjust_color(creme, -14)) or black

	local panel = {
		title = pick(opts.title, "Run"),
		width = pick(opts.width, 540),
		height = pick(opts.height, 120),
		footer_h = pick(opts.footer_h, 0),
		radius = pick(opts.panel_radius, 12),
		border_w = pick(opts.panel_border_width, 1),
		border = pick(opts.panel_border, border),
		bg = pick(opts.panel_bg, (H.rgba and H.rgba(black, 0.66)) or "#000000AA"),
		header_h = pick(opts.panel_header_h, 28),
		header_bg = pick(opts.panel_header_bg, C.header_bg or "#235CDB"),
		header_fg = pick(opts.panel_header_fg, C.header_fg or "#FFF"),
		body_bg = pick(opts.panel_body_bg, C.body_bg or "#1d2f6f"),
		body_fg = pick(opts.panel_body_fg, C.body_fg or "#FFF"),
	}

	-- Search-Bar (innen)
	local search = {
		sizes = {
			height = math.max(16, math.floor((panel.footer_h > 0 and panel.footer_h or panel.height) / 3 + 0.5)),
			width_expanded = pick(opts.search and opts.search.width_expanded, 400),
			width_collapsed = pick(opts.search and opts.search.width_collapsed, 400),
		},
		colors = {
			bg_active = pick(opts.search and opts.search.colors and opts.search.colors.bg, C.creme or "#fff"),
			fg_active = pick(opts.search and opts.search.colors and opts.search.colors.fg, C.black or "#000"),
			bg_collapsed = "#00000000",
			cursor_bg = "#00000000",
			cursor_fg = C.black or "#000",
		},
		layout = {
			left = 12,
			right = 12,
			top = 8,
			bottom = 8,
			spacing = 8,
		},
		web = {
			browser = (opts.search and opts.search.web and opts.search.web.browser) or "firefox",
			engine = (opts.search and opts.search.web and opts.search.web.engine) or "https://duckduckgo.com/?q=%s",
		},
		prefix = {
			local_mode = "/", -- files
			web_mode = "?", -- web
			run_mode = "", -- apps (default)
		},
	}

	return { panel = panel, search = search }
end

return M
