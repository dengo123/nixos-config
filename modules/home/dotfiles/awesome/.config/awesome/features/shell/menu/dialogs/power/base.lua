-- features/shell/menu/dialogs/parts/base.lua
-- Reiner Dialog-Container:
--  - baut Header, Body (inkl. Actions-Grid), Footer
--  - KEINE Popup-/Backdrop-/ESC-Logik (das macht parts/popup.lua)

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local Popup = require("features.shell.menu.dialogs.parts.popup")

local Base = {}

-- utils
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- helpers for precise row layout (keine Popup-Logik)
local function spacer_exact(px)
	return wibox.widget({
		wibox.widget({}),
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end

local function fixed_cell(widget, width)
	return wibox.widget({
		{ widget, halign = "center", valign = "center", widget = wibox.container.place },
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

local function build_precise_fixed_row(icon_cells, targets, icon_px_w, place_px_w)
	local n = #icon_cells
	local S = {}
	if n == 1 then
		local x = targets[1] * place_px_w
		S[1] = math.max(0, x - icon_px_w / 2)
		S[2] = math.max(0, place_px_w - (S[1] + icon_px_w))
	else
		local sum = 0
		S[1] = math.max(0, targets[1] * place_px_w - icon_px_w / 2)
		sum = sum + S[1]
		for i = 2, n do
			local left_needed = targets[i] * place_px_w - icon_px_w / 2
			local si = left_needed - (sum + (i - 1) * icon_px_w)
			S[i] = math.max(0, si)
			sum = sum + S[i]
		end
		S[n + 1] = math.max(0, place_px_w - (sum + n * icon_px_w))
	end

	local row = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	for i = 1, n do
		row:add(spacer_exact(S[i]))
		row:add(icon_cells[i])
	end
	row:add(spacer_exact(S[n + 1] or 0))
	return row
end

-- Öffentlichkeit: generischer Choice-Dialog (Actions als Icon-Buttons)
function Base.choice(opts)
	opts = opts or {}

	-- Theme kommt von außen bereits „resolved“ (z. B. Theme.get(...) in power/init.lua)
	local th = opts.theme or {}

	-- Zielgröße & Segmente
	local DIALOG_W = pick(th.dialog_w, 560)
	local DIALOG_H = pick(th.dialog_h, 360)

	local HR = pick(th.header_ratio, 0.18)
	local FR = pick(th.footer_ratio, 0.18)
	local H_HEADER = math.floor(DIALOG_H * HR)
	local H_FOOT = math.floor(DIALOG_H * FR)
	local H_BODY = DIALOG_H - H_HEADER - H_FOOT

	-- Icon/Cell-Geometrie
	local base_side = math.min(DIALOG_W, DIALOG_H)
	local ICON_SIZE_RAW = math.floor(base_side * pick(th.icon_ratio, 0.20))
	local PAD_V = pick(th.pad_v, 14)
	local ICON_MAX = math.max(8, H_BODY - 2 * PAD_V)
	local ICON_SIZE = math.min(ICON_SIZE_RAW, ICON_MAX)

	local icon_pad = pick(th.icon_pad, 6)
	local cell_pad = pick(th.icon_cell_pad, 6)
	local cell_extra = pick(th.icon_cell_extra_w, 12)
	local ICON_CELL_W = ICON_SIZE + icon_pad * 2 + cell_pad * 2 + cell_extra

	-- Header
	local header_content = W.mk_header_content(opts.title or "", th)
	local header_container = wibox.widget({
		{
			header_content,
			left = pick(th.header_pad_h, th.pad_h, 12),
			right = pick(th.header_pad_h, th.pad_h, 12),
			widget = wibox.container.margin,
		},
		bg = pick(th.header_bg, "#235CDB"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Actions → Icon-Zellen
	local actions = opts.actions or {}
	local n = #actions
	local built_cells = {}
	local close_ref = function() end -- wird nach Popup.show gesetzt

	for i = 1, n do
		local a = actions[i]
		local iconw = W.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = ICON_SIZE,
			label = a.label,
			th = th,
			on_press = function()
				if a.on_press then
					a.on_press(close_ref)
				end
			end,
		})
		built_cells[i] = fixed_cell(iconw, ICON_CELL_W)
	end

	-- Body-Reihe mittig verteilen
	local PAD_H = pick(th.pad_h, 16)
	local place_w = DIALOG_W - 2 * PAD_H
	local targets = {}
	for i = 1, n do
		targets[i] = (i - 0.5) / math.max(n, 1)
	end

	local icons_row = (n > 0) and build_precise_fixed_row(built_cells, targets, ICON_CELL_W, place_w)
		or wibox.widget({ layout = wibox.layout.fixed.horizontal })

	local body_with_margins = wibox.widget({
		icons_row,
		left = PAD_H,
		right = PAD_H,
		top = PAD_V,
		bottom = PAD_V,
		widget = wibox.container.margin,
	})
	local body_centered = wibox.widget({
		body_with_margins,
		halign = "center",
		valign = "center",
		widget = wibox.container.place,
	})
	local body_container = wibox.widget({
		body_centered,
		bg = pick(th.body_bg, "#00000000"),
		fg = pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Footer (Cancel rechts, vertikal mittig). Button-Logik steckt in widgets.lua;
	-- hier nur Platzierung + Breiten-Constraint.
	local cancel_btn = W.mk_cancel_button((th.cancel_label or "Cancel"), function() end, th)
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
		right = pick(th.footer_pad_h, th.pad_h, 12),
		widget = wibox.container.margin,
	})
	local footer_container = wibox.widget({
		{
			footer_right,
			top = pick(th.footer_pad_v, 8),
			bottom = pick(th.footer_pad_v, 8),
			widget = wibox.container.margin,
		},
		bg = pick(th.footer_bg, "#235CDB"),
		fg = pick(th.footer_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Fixhöhen
	local header_fixed = wibox.widget({
		header_container,
		strategy = "exact",
		height = H_HEADER,
		widget = wibox.container.constraint,
	})
	local body_fixed = wibox.widget({
		body_container,
		strategy = "exact",
		height = H_BODY,
		widget = wibox.container.constraint,
	})
	local footer_fixed = wibox.widget({
		footer_container,
		strategy = "exact",
		height = H_FOOT,
		widget = wibox.container.constraint,
	})

	-- Formular (ohne Border/Shape/Popup-Logik!)
	local form_widget = wibox.widget({
		header_fixed,
		body_fixed,
		footer_fixed,
		layout = wibox.layout.fixed.vertical,
	})

	-- Delegation an Popup (Popup übernimmt ESC/Backdrop/Guards)
	local handle = Popup.show(form_widget, th, {
		width = DIALOG_W,
		height = DIALOG_H,
		close_on_escape = true, -- ESC schließt
		close_on_backdrop = false, -- Klicks werden geblockt, schließen nicht
	})

	-- close() an Actions/Cancel durchreichen
	close_ref = handle.close
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		handle.close()
	end)))

	return handle
end

return Base
