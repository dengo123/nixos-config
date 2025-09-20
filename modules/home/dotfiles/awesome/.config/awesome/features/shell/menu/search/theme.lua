-- features/shell/menu/search/theme.lua
-- Zentrale Theme-/Layout-Defaults für die Search-Bar
-- API:
--   local Theme = require("features.shell.menu.search.theme")
--   local t = Theme.get({
--     -- optionale Overrides (siehe defaults unten)
--   })
--   -- oder: Theme.merge(a, b)  -- flaches Merge

local Theme = {}

-- flaches Merge (b überschreibt a)
function Theme.merge(a, b)
	a = type(a) == "table" and a or {}
	b = type(b) == "table" and b or {}
	local out = {}
	for k, v in pairs(a) do
		out[k] = v
	end
	for k, v in pairs(b) do
		out[k] = v
	end
	return out
end

-- sichere Auswahl: nimm das erste nicht-nil
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- Höhe aus Footer-Höhe ableiten (1/3, min 16px)
local function derive_height_px(footer_h)
	local FOOT_H = tonumber(footer_h) or 48
	return math.max(16, math.floor(FOOT_H / 3 + 0.5))
end

-- Default-Palette + Geometrie (bewusst minimal gehalten)
Theme.defaults = {
	-- Geometrie
	footer_h = 48, -- Basis zur Höhenableitung
	height_px = nil, -- wenn nil -> aus footer_h abgeleitet
	width_expanded = 220,
	width_collapsed = 200,

	-- Abstände & Layout
	margin_left = 10,
	margin_right = 10,
	margin_top = 4,
	margin_bottom = 4,
	prefix_spacing = 8, -- Abstand zwischen Präfix-Label und Textbox

	-- Farben (aktiv/kollabiert)
	colors = {
		bg = "#FFFFFF",
		fg = "#000000",
		bg_collapsed = "#00000000", -- transparent in kollabiertem Zustand
		-- Cursor (Textbox)
		cursor_bg = "#00000000",
		cursor_fg = "#000000",
	},

	-- (Optional) künftige Erweiterungen:
	-- radius = 8, border_w = 0, border_color = "#00000000",
}

-- Liefert ein vollständig aufgelöstes Theme:
-- - `overrides` überschreibt defaults
-- - `shared` kann globale UI-Defaults liefern (z.B. aus deinem Menü-Theme)
function Theme.get(overrides, shared)
	overrides = overrides or {}
	shared = shared or {}

	-- 1) grobe Defaults
	local base = Theme.merge(Theme.defaults, {})

	-- 2) shared (z. B. globale Farben), aber nur wenn angegeben
	--    Nutze pick() um sinnvolle Fallback-Reihenfolge zu sichern.
	local merged_colors = {
		bg = pick(overrides.colors and overrides.colors.bg, shared.search_bg, shared.bg, base.colors.bg),
		fg = pick(overrides.colors and overrides.colors.fg, shared.search_fg, shared.fg, base.colors.fg),
		bg_collapsed = pick(
			overrides.colors and overrides.colors.bg_collapsed,
			shared.search_bg_collapsed,
			base.colors.bg_collapsed
		),
		cursor_bg = pick(overrides.colors and overrides.colors.cursor_bg, base.colors.cursor_bg),
		cursor_fg = pick(overrides.colors and overrides.colors.cursor_fg, base.colors.cursor_fg),
	}

	-- 3) finalen Datensatz bauen
	local t = {
		footer_h = pick(overrides.footer_h, shared.footer_h, base.footer_h),
		height_px = pick(overrides.height_px, nil), -- evtl. später gesetzt
		width_expanded = pick(overrides.width_expanded, base.width_expanded),
		width_collapsed = pick(overrides.width_collapsed, base.width_collapsed),

		margin_left = pick(overrides.margin_left, base.margin_left),
		margin_right = pick(overrides.margin_right, base.margin_right),
		margin_top = pick(overrides.margin_top, base.margin_top),
		margin_bottom = pick(overrides.margin_bottom, base.margin_bottom),
		prefix_spacing = pick(overrides.prefix_spacing, base.prefix_spacing),

		colors = merged_colors,

		-- radius        = pick(overrides.radius,        base.radius),
		-- border_w      = pick(overrides.border_w,     base.border_w),
		-- border_color  = pick(overrides.border_color, base.border_color),
	}

	-- Höhe ableiten, falls nicht explizit gesetzt
	t.height_px = t.height_px or derive_height_px(t.footer_h)

	return t
end

return Theme
