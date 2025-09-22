-- ~/.config/awesome/features/shell/menu/parts/footer.lua
local wibox = require("wibox")
local P = require("features.shell.menu.widgets") -- enthält power_bar()
local Search = require("features.shell.menu.search") -- deine Search-Implementierung

local Footer = {}

-- Kompatibel:
--   Footer.build(power_items, t)
--   Footer.build({ power_items = ..., t = ..., search = {...} })
function Footer.build(arg1, arg2)
	-- ---- Compat Layer ---------------------------------------------------------
	local opts
	if type(arg1) == "table" and (arg1.power_items or arg1.t or arg1.search) then
		opts = arg1
	else
		opts = { power_items = arg1, t = arg2 }
	end
	opts.t = opts.t or {}
	local t = opts.t

	-- ---------------------------------------------------------------------------
	-- Footer-Grundlayout (nur Container)
	-- ---------------------------------------------------------------------------
	local FOOTER_H = t.footer_h or 48
	local PAD_T = t.footer_pad_t or 6
	local PAD_B = t.footer_pad_b or 6
	local inner_h = math.max(FOOTER_H - PAD_T - PAD_B, 1)

	local FOOTER_BG = t.footer_bg or t.bg or "#235CDB"
	local FOOTER_FG = t.footer_fg or t.fg or "#FFFFFF"

	-- ---------------------------------------------------------------------------
	-- Search: komplett gekapselt im Modul features/shell/menu/search/init.lua
	-- ---------------------------------------------------------------------------
	local search_cfg = opts.search or {}
	local search_inst = Search.new({
		footer_h = FOOTER_H,
		width_expanded = search_cfg.width_expanded or 220,
		width_collapsed = search_cfg.width_collapsed or 200,
		colors = {
			bg = (search_cfg.colors and search_cfg.colors.bg) or "#FFFFFF",
			fg = (search_cfg.colors and search_cfg.colors.fg) or "#000000",
			bg_collapsed = (search_cfg.colors and search_cfg.colors.bg_collapsed) or "#00000000",
		},
		web = {
			browser = (search_cfg.web and search_cfg.web.browser) or "firefox",
			engine = (search_cfg.web and search_cfg.web.engine) or "https://duckduckgo.com/?q=%s",
		},
	})

	-- ---------------------------------------------------------------------------
	-- Power-Buttons rechts (UI-only; Clicks -> Actions)
	-- ---------------------------------------------------------------------------
	local powers_right = P.power_bar(opts.power_items or {}, t, { inner_h = inner_h })

	-- ---------------------------------------------------------------------------
	-- Footer-Row & Container
	-- ---------------------------------------------------------------------------
	local row = wibox.widget({
		search_inst.widget, -- links: Search-Bar
		nil, -- mitte: leer/Stretch
		powers_right, -- rechts: Power-Leiste
		expand = "inside",
		layout = wibox.layout.align.horizontal,
	})

	local footer = wibox.widget({
		{
			row,
			left = t.footer_pad_l or 10,
			right = t.footer_pad_r or 10,
			top = PAD_T,
			bottom = PAD_B,
			widget = wibox.container.margin,
		},
		forced_height = FOOTER_H,
		bg = FOOTER_BG,
		fg = FOOTER_FG,
		widget = wibox.container.background,
	})

	-- ---------------------------------------------------------------------------
	-- API: Search-Funktionen in bestehende Menü-API injizieren (nicht überschreiben)
	-- ---------------------------------------------------------------------------
	local api = rawget(_G, "__menu_api") or {}

	api.focus_search = function()
		search_inst.api.focus_local()
	end
	api.focus_search_web = function()
		search_inst.api.focus_web()
	end
	api.cancel_search = function()
		search_inst.api.cancel()
	end
	api.is_search_active = function()
		return search_inst.api.is_active()
	end
	api.is_search_collapsed = function()
		return search_inst.api.is_collapsed()
	end
	api.set_search_engine = function(url_fmt)
		search_inst.api.set_engine(url_fmt)
	end
	api.set_search_browser = function(bin)
		search_inst.api.set_browser(bin)
	end

	_G.__search_api = search_inst.api -- optional

	return footer, api
end

return Footer
