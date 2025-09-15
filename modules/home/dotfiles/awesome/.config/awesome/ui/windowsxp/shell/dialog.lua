-- ui/windowsxp/shell/dialog.lua
-- Windows XP "Luna"-Style Theme für Shell-Dialoge
-- Exportiert:
--   M.defaults                 -- Basis (Luna Blue)
--   M.variants                 -- { "luna-blue", "luna-olive", "luna-silver" }
--   M.get(variant, overrides)  -- Theme holen (Variante + Overrides gemerged)
--   M.scale(theme, factor)     -- Größen im Theme skalieren (pure)
--   M.with_alpha(hex, aa)      -- "#RRGGBB" + "AA" -> "#RRGGBBAA"

local M = {}

-- kleine Helfer ---------------------------------------------------------------

local function shallow_copy(tbl)
	local out = {}
	for k, v in pairs(tbl or {}) do
		out[k] = v
	end
	return out
end

local function merge(a, b)
	local out = shallow_copy(a)
	for k, v in pairs(b or {}) do
		out[k] = v
	end
	return out
end

-- Hex-Alpha anhängen: with_alpha("#RRGGBB", "AA") => "#RRGGBBAA"
function M.with_alpha(hex, aa)
	if not hex or not aa then
		return hex
	end
	-- akzeptiere bereits #RRGGBBAA → ersetze Alpha
	if #hex == 9 then
		return hex:sub(1, 7) .. aa
	end
	return hex .. aa
end

-- Baseline (Luna Blue) -------------------------------------------------------
-- Hinweis: Farbtöne sind bewusst "mittel" gesetzt (statt Gradients),
-- um sie stabil in Flat-Widgets zu nutzen. Gradients können später
-- in den Komponenten umgesetzt werden, falls gewünscht.

M.defaults = {
	-- Abmessungen/Geometrie
	dialog_w = 560,
	dialog_h = 360,
	radius = 12,

	-- Flächenfarben
	header_bg = "#245CDC", -- Luna Blue Title-Bar Mittelwert
	header_fg = "#FFFFFF",
	body_bg = "#D6E6FF", -- sehr helles XP-Blau
	body_fg = "#000000",
	footer_bg = "#245CDC",
	footer_fg = "#FFFFFF",

	-- Gesamtrahmen/Backdrop
	dialog_bg = "#00000000", -- transparent (vom Base-Container gerendert)
	backdrop = "#00000066", -- halbtransparentes Schwarz

	-- Spacing
	pad_h = 16, -- horizontaler Innenabstand
	pad_v = 14, -- vertikaler Innenabstand
	icons_spacing = 24, -- Abstand zwischen Icons im Body

	-- Cancel-Button (rechts im Footer)
	cancel_bg = "#F5F5EE", -- XP-beige
	cancel_fg = "#000000",
	cancel_hover_bg = "#F5F5EE",
	cancel_hover_border = "#3A6EA5", -- XP Mittelblau fürs Hover-Outline

	-- Icon-Hover
	icon_hover_bg = "#FFFFFF20",
	icon_hover_border = "#2B5B88",
}

-- Varianten (Luna Olive / Silver) --------------------------------------------

M.variants = {
	["luna-blue"] = {}, -- identisch zu defaults

	["luna-olive"] = {
		header_bg = "#5F8A1F",
		footer_bg = "#5F8A1F",
		body_bg = "#ECF5DC",
		cancel_hover_border = "#6E8F2B",
		icon_hover_border = "#4C6C18",
	},

	["luna-silver"] = {
		header_bg = "#7F9DB9",
		footer_bg = "#7F9DB9",
		body_bg = "#EEF2F7",
		cancel_hover_border = "#6F8BA7",
		icon_hover_border = "#5A768F",
	},
}

-- API ------------------------------------------------------------------------

-- Größen skalieren (pure): gibt NEUE Tabelle zurück
function M.scale(theme, factor)
	local t = shallow_copy(theme or M.defaults)
	local f = tonumber(factor) or 1.0
	t.dialog_w = math.floor((t.dialog_w or M.defaults.dialog_w) * f + 0.5)
	t.dialog_h = math.floor((t.dialog_h or M.defaults.dialog_h) * f + 0.5)
	t.radius = math.floor((t.radius or M.defaults.radius) * f + 0.5)
	t.pad_h = math.floor((t.pad_h or M.defaults.pad_h) * f + 0.5)
	t.pad_v = math.floor((t.pad_v or M.defaults.pad_v) * f + 0.5)
	t.icons_spacing = math.floor((t.icons_spacing or M.defaults.icons_spacing) * f + 0.5)
	return t
end

-- zentrales Abrufen: Variante + Overrides mergen
-- usage:
--   local th = DialogTheme.get("luna-olive", { dialog_w = 640, backdrop = DialogTheme.with_alpha("#000000","80") })
function M.get(variant, overrides)
	local v = M.variants[variant or "luna-blue"] or {}
	local base = merge(M.defaults, v)
	return merge(base, overrides)
end

-- (optional) nur Merge der Defaults + Overrides, ohne Variante
function M.merge(overrides)
	return merge(M.defaults, overrides)
end

return M
