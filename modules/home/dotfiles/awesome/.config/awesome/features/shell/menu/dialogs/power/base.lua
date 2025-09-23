-- ~/.config/awesome/features/shell/menu/dialogs/parts/base.lua
-- Komponiert Header / Body / Footer. Kein Lifecycle (liegt in parts/popup.lua).

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local H = require("features.shell.menu.dialogs.parts.helpers")
local Popup = require("features.shell.menu.dialogs.parts.popup")

local Base = {}

-- --- Theme-Resolve ----------------------------------------------------------
local function resolve_theme(theme)
	if type(theme) == "function" then
		local ok, t = pcall(theme)
		if ok and type(t) == "table" then
			return t
		end
	elseif type(theme) == "table" then
		return theme
	end
	local ui = rawget(_G, "ui")
	if ui and type(ui.theme) == "table" then
		return ui.theme
	end
	return {}
end

local function num(x, fb)
	x = tonumber(x)
	return x ~= nil and x or fb
end

-- --- Public -----------------------------------------------------------------
function Base.choice(opts)
	opts = opts or {}

	-- Theme zuerst sauber auflösen
	local th = resolve_theme(opts.theme)

	-- Größen & Geometrie
	local DIALOG_W = num(H.pick(th.dialog_w, 560), 560)
	local DIALOG_H = num(H.pick(th.dialog_h, 360), 360)
	local geom = H.compute_icon_metrics(th, DIALOG_W, DIALOG_H) -- liefert seg/pads etc.

	-- ========================= Header =========================================
	local header = wibox.widget({
		{
			W.mk_header_content(opts.title or "", th),
			left = num(H.pick(th.header_pad_h, th.pad_h, 12), 12),
			right = num(H.pick(th.header_pad_h, th.pad_h, 12), 12),
			widget = wibox.container.margin,
		},
		bg = H.pick(th.header_bg, "#235CDB"),
		fg = H.pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- ========================= Body ===========================================
	-- Body Core: entweder externer body_widget oder Icon-Grid
	local actions = opts.actions or {}
	local body_core

	local close_ref = function() end -- später mit echtem handle.close belegt

	if opts.body_widget then
		body_core = opts.body_widget
	else
		local cells = {}
		for i, a in ipairs(actions) do
			local btn = W.mk_icon_button({
				icon = a.icon,
				emoji = a.emoji,
				emoji_font = a.emoji_font,
				size = geom.icon_size,
				label = a.label,
				th = th,
				on_press = function()
					if a.on_press then
						a.on_press(close_ref)
					end
				end,
			})
			cells[i] = H.fixed_cell(btn, geom.icon_cell_w)
		end
		local targets = H.targets_linear(#cells)
		body_core = (#cells > 0) and H.build_even_row(cells, targets, geom.icon_cell_w, geom.place_w)
			or wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end

	-- WICHTIG: Padding INSIDE der Background-Box, damit kein „Rand“ um den Body entsteht.
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
		right = num(H.pick(th.footer_pad_h, th.pad_h, 12), 12),
		widget = wibox.container.margin,
	})

	local footer = wibox.widget({
		{
			footer_right,
			top = num(H.pick(th.footer_pad_v, 8), 8),
			bottom = num(H.pick(th.footer_pad_v, 8), 8),
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

	-- Echten close in die Action-Handler injizieren
	close_ref = handle.close

	return handle
end

return Base
