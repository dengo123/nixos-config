-- ~/.config/awesome/features/shell/menu/dialogs/parts/base.lua
-- Komponiert Header / Body / Footer. Kein Lifecycle (liegt in parts/popup.lua).

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local H = require("features.shell.menu.dialogs.parts.helpers")
local Popup = require("features.shell.menu.dialogs.parts.popup")
local Theme = require("features.shell.menu.dialogs.parts.theme")
local Icons = require("features.shell.menu.dialogs.parts.icons")

local Base = {}

-- --- Theme-Resolve ----------------------------------------------------------

-- Fallback-Merge, falls Theme.merge nicht vorhanden ist (flaches Merge: a < b)
local function shallow_merge(a, b)
	local out = {}
	if type(a) == "table" then
		for k, v in pairs(a) do
			out[k] = v
		end
	end
	if type(b) == "table" then
		for k, v in pairs(b) do
			out[k] = v
		end
	end
	return out
end

local function resolve_theme(theme_overrides)
	-- 1) Overrides aus Fn/Table ziehen
	local overrides = {}
	if type(theme_overrides) == "function" then
		local ok, t = pcall(theme_overrides)
		if ok and type(t) == "table" then
			overrides = t
		end
	elseif type(theme_overrides) == "table" then
		overrides = theme_overrides
	end

	-- 2) ui.theme als Basis (falls vorhanden)
	local ui_base = {}
	local ui = rawget(_G, "ui")
	if ui and type(ui.theme) == "table" then
		ui_base = ui.theme
	end

	-- 3) Mergen + Defaults via Theme.get/merge (mit Fallback)
	local merged
	if Theme and type(Theme.merge) == "function" then
		merged = Theme.merge(ui_base, overrides)
	else
		merged = shallow_merge(ui_base, overrides)
	end

	if Theme and type(Theme.get) == "function" then
		return Theme.get(merged)
	end
	return merged or {}
end

-- --- Public -----------------------------------------------------------------
function Base.choice(opts)
	opts = opts or {}

	-- Theme auflösen (zentral hier)
	local th = resolve_theme(opts.theme)

	-- Größen & Geometrie
	local DIALOG_W = H.num(H.pick(th.dialog_w, 560), 560)
	local DIALOG_H = H.num(H.pick(th.dialog_h, 360), 360)
	local geom = H.compute_icon_metrics(th, DIALOG_W, DIALOG_H) -- seg/pads etc.

	-- ========================= Header =========================================
	local header = wibox.widget({
		{
			W.mk_header_content(opts.title or "", th),
			left = H.num(H.pick(th.header_pad_h, th.pad_h, 12), 12),
			right = H.num(H.pick(th.header_pad_h, th.pad_h, 12), 12),
			widget = wibox.container.margin,
		},
		bg = H.pick(th.header_bg, "#235CDB"),
		fg = H.pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- ========================= Body ===========================================
	-- Body Core: entweder externer body_widget oder Icon-Row über Icons.actions_row
	local body_core
	local close_ref = function() end -- später mit echtem handle.close belegt

	if opts.body_widget then
		body_core = opts.body_widget
	else
		body_core = Icons.actions_row(opts.actions or {}, th, geom, function()
			return close_ref
		end)
	end

	-- Padding INSIDE der Background-Box (keine „äußeren“ Ränder um den Body)
	local body = wibox.widget({
		{
			{ body_core, halign = "center", valign = "center", widget = wibox.container.place },
			left = geom.pad_h,
			right = geom.pad_h,
			top = geom.pad_v,
			bottom = geom.pad_v,
			widget = wibox.container.margin,
		},
		bg = H.pick(th.body_bg, "#00000000"),
		fg = H.pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- ========================= Footer =========================================
	local cancel_btn = W.mk_cancel_button(th.cancel_label or "Cancel", nil, th)
	local target_w = th.cancel_width or (DIALOG_W > 0 and math.floor(DIALOG_W / 7) or nil)
	if target_w then
		cancel_btn = wibox.widget({
			cancel_btn,
			strategy = "exact",
			width = target_w,
			widget = wibox.container.constraint,
		})
	end

	local footer_right = wibox.widget({
		{ cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
		right = H.num(H.pick(th.footer_pad_h, th.pad_h, 12), 12),
		widget = wibox.container.margin,
	})

	local footer = wibox.widget({
		{
			footer_right,
			top = H.num(H.pick(th.footer_pad_v, 8), 8),
			bottom = H.num(H.pick(th.footer_pad_v, 8), 8),
			widget = wibox.container.margin,
		},
		bg = H.pick(th.footer_bg, "#235CDB"),
		fg = H.pick(th.footer_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- ========================= Fixhöhen / Stack ================================
	local seg = geom.seg

	local header_fixed = wibox.widget({
		header,
		strategy = "exact",
		height = seg.header,
		widget = wibox.container.constraint,
	})
	local body_fixed = wibox.widget({
		body,
		strategy = "exact",
		height = seg.body,
		widget = wibox.container.constraint,
	})
	local footer_fixed = wibox.widget({
		footer,
		strategy = "exact",
		height = seg.footer,
		widget = wibox.container.constraint,
	})

	local stack = wibox.widget({
		header_fixed,
		body_fixed,
		footer_fixed,
		layout = wibox.layout.fixed.vertical,
	})

	-- ========================= Popup öffnen (Lifecycle in parts/popup.lua) =====
	local handle = Popup.show(stack, th, {
		width = DIALOG_W,
		height = DIALOG_H,
		close_on_escape = true,
		close_on_backdrop = false,
		group = "dialogs", -- optional: gruppenweises Schließen
	})

	-- Cancel → close
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		handle.close()
	end)))

	-- Echten close in die Action-Handler injizieren (lazy via get_close_ref)
	close_ref = handle.close

	return handle
end

return Base
