-- ~/.config/awesome/features/shell/menu/parts/footer.lua
local wibox = require("wibox")

local P = require("features.shell.menu.widgets") -- Buttons/Leisten
local Dialogs = require("features.shell.menu.dialogs") -- Power-/Logout-Dialoge
local Search = require("features.shell.menu.search") -- <- dein neues Search-Modul (init.lua)

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
	-- Footer-Grundlayout (nur Container – KEINE Search-Logik/Theming hier!)
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
	-- Power-Buttons rechts (komplett in widgets.lua gelöst)
	-- ---------------------------------------------------------------------------
	local powers_right = P.power_bar(opts.power_items or {}, t, {
		inner_h = inner_h,
		dialogs = Dialogs,
	})

	-- ---------------------------------------------------------------------------
	-- Footer-Row & Container
	-- ---------------------------------------------------------------------------
	local row = wibox.widget({
		search_inst.widget, -- links: Search-Bar (fertiges Widget aus dem Modul)
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
	-- API-Passthrough (für Keybinds/Signals)
	-- ---------------------------------------------------------------------------
	local api = {
		-- Search-Steuerung bequem durchreichen:
		focus_search = function()
			search_inst.api.focus_local()
		end,
		focus_search_web = function()
			search_inst.api.focus_web()
		end,
		cancel_search = function()
			search_inst.api.cancel()
		end,
		is_search_active = function()
			return search_inst.api.is_active()
		end,
		is_collapsed = function()
			return search_inst.api.is_collapsed()
		end,

		-- Optional: Engine/Browser on-the-fly wechseln
		set_search_engine = function(url_fmt)
			search_inst.api.set_engine(url_fmt)
		end,
		set_search_browser = function(bin)
			search_inst.api.set_browser(bin)
		end,

		-- Menü-API: Toggle (z. B. für <Super+Space>)
		toggle = function()
			if search_inst.api.is_active() then
				search_inst.api.cancel()
			else
				search_inst.api.focus_local()
			end
		end,
	}

	-- globale API einmalig veröffentlichen (Keybinds können darauf zugreifen)
	_G.__menu_api = api

	return footer, api
end

return Footer
