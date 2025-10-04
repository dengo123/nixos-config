-- ~/.config/awesome/ui/theme/run.lua
-- Theme für den Run-Launcher (Popup + Searchbar)
-- Liefert { panel = {...}, search = {...} } und sonst nichts.
-- Keine harten beautiful-Writes; alles wird vom Launcher konsumiert.

local Colors = require("ui.colors")
local Helpers = require("ui.helpers")

local M = {}

--- get(overrides?) -> { panel = {...}, search = {...} }
--  Achtung: Nur Felder zurückgeben, die der Launcher wirklich nutzt.
function M.get(overrides)
	overrides = overrides or {}
	local C, H, d = Colors.get(), Helpers, Helpers.dpi

	-- ========================= Panel (Container) =========================
	-- Struktur orientiert sich an deinem power/container.lua.
	local panel_defaults = {
		-- Titelzeile
		title = "Run",
		header_h = d(36),
		header_bg = C.blue_luna,
		header_fg = C.white,
		header_font = "Sans",
		header_font_size = d(14),
		header_pad_l = d(12),
		header_pad_r = d(12),
		header_pad_v = d(6),

		-- Body / Footer
		body_bg = C.creme,
		body_fg = C.black,
		footer_h = d(52),
		footer_bg = C.creme,
		footer_fg = C.black,

		-- Außenmaße / Rand
		width = d(520),
		height = d(280),
		pad_h = d(14),
		pad_v = d(12),

		-- Rahmen um das gesamte Panel
		panel_radius = d(12),
		panel_bg = C.blue_luna, -- Hintergrund außerhalb von Header/Body/Footer
		panel_border = C.blue_luna,
		panel_border_width = d(2),
	}

	-- ========================= Searchbar (im Body) ======================
	-- Struktur deckt sich mit run/view.lua und run/init.lua (UI-Mapping).
	local search_defaults = {
		sizes = {
			height = d(24), -- fixe Höhe der weißen Leiste
		},
		colors = {
			bg_active = C.white, -- Feldhintergrund
			fg_active = C.black, -- Textfarbe
			cursor_bg = C.black, -- Cursor-Block-Hintergrund (für set_cursor_style("block"))
			cursor_fg = C.white, -- Cursor-Block-Vordergrund
		},
		layout = {
			left = d(6),
			right = d(12),
			top = d(6),
			bottom = d(6),
			spacing = d(8),
		},

		-- Schwarzer Rahmen um die Searchbar
		border_w = d(1),
		border_color = C.black,

		-- Linkes Label „Open:“ vor der weißen Leiste
		label_open_text = "Open:",
		-- label_open_width = d(24),

		-- Hinweiszeile über der Leiste (Icon + Text)
		hint = {
			show = true,
			icon = "🛈",
			text = "Ctrl + / = Files   ·   Ctrl + Shift + / = Web   ·   Esc = Close",
			fg = C.black,
			bg = C.creme, -- gleiche Body-Farbe wirkt harmonisch
			font = "Sans",
			size = d(10),
			spacing = d(6),
		},

		-- Präfixe je Modus (wird links im Feld angezeigt)
		prefix = {
			run_mode = "run", -- klassischer Run
			local_mode = "files", -- Files
			web_mode = "web", -- Web
		},

		-- Web-Ziel
		web = {
			browser = "firefox",
			engine = "https://duckduckgo.com/?q=%s",
		},
	}

	-- ========================= Merge mit overrides ======================
	local panel = H.merge(panel_defaults, (overrides.panel or {}))
	local search = H.merge(search_defaults, (overrides.search or {}))

	return {
		panel = panel,
		search = search,
	}
end

-- kompatibler Alias (falls an anderer Stelle .resolve genutzt wird)
M.resolve = M.get

-- Optional: init(cfg) NICHT erforderlich; wir setzen hier nichts auf beautiful.*
-- Ein leerer init wäre möglich, aber unnötig.
-- function M.init(_) end

return M
