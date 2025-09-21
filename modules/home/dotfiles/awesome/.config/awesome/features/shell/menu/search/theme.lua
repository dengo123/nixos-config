-- features/shell/menu/search/theme.lua
-- Liefert ein aufgelöstes Theme-Objekt über Theme.resolve(opts)

local Theme = {}

local function pick(a, b)
	return (a ~= nil) and a or b
end

function Theme.resolve(opts)
	opts = opts or {}

	-- Größen (Höhe aus footer_h ableiten: 1/3, min 16)
	local footer_h = pick(opts.footer_h, 48)
	local height = math.max(16, math.floor(footer_h / 3 + 0.5))
	local width_exp = pick(opts.width_expanded, 200)
	local width_col = pick(opts.width_collapsed, width_exp)

	-- Farben
	local c = opts.colors or {}
	local bg_active = pick(c.bg, "#FFFFFF")
	local fg_active = pick(c.fg, "#000000")
	local bg_collapsed = pick(c.bg_collapsed, "#00000000")
	local cursor_bg = pick(c.cursor_bg, "#00000000")
	local cursor_fg = pick(c.cursor_fg, fg_active)

	-- Layout
	local l = opts.layout or {}
	local left = pick(l.left, 10)
	local right = pick(l.right, 10)
	local top = pick(l.top, 4)
	local bottom = pick(l.bottom, 4)
	local spacing = pick(l.spacing, 8)

	-- Web (Browser + Suchmaschine)
	local w = opts.web or {}
	local browser = pick(w.browser, "firefox")
	local engine = pick(w.engine, "https://duckduckgo.com/?q=%s")

	-- Präfixe (optional)
	local p = opts.prefix or {}
	local prefix_local = pick(p.local_mode, "/")
	local prefix_web = pick(p.web_mode, "?")

	return {
		sizes = {
			height = height,
			width_expanded = width_exp,
			width_collapsed = width_col,
		},
		colors = {
			bg_active = bg_active,
			fg_active = fg_active,
			bg_collapsed = bg_collapsed,
			cursor_bg = cursor_bg,
			cursor_fg = cursor_fg,
		},
		layout = {
			left = left,
			right = right,
			top = top,
			bottom = bottom,
			spacing = spacing,
		},
		web = {
			browser = browser,
			engine = engine,
		},
		prefix = {
			local_mode = prefix_local,
			web_mode = prefix_web,
		},
	}
end

return Theme
