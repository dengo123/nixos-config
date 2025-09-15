-- ~/.config/awesome/features/shell/menu/shared/dialogs/base.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local W = require("features.shell.menu.shared.dialogs.widgets")

local Base = {}

-- Icon in fester Zellbreite mittig halten (verhindert Zusammendrücken)
local function fixed_cell(widget, width)
	return wibox.widget({
		{
			widget,
			halign = "center",
			valign = "center",
			widget = wibox.container.place,
		},
		strategy = "exact",
		width = width,
		widget = wibox.container.constraint,
	})
end

-- exakten Spacer in Pixeln bauen (leer, aber mit fester Breite)
local function spacer_exact(px)
	return wibox.widget({
		wibox.widget({}), -- leerer Inhalt
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end

-- baue Reihe: [S1][I1][S2][I2]...[Sn][In][S_{n+1}] als fixed.horizontal
-- targets: gewünschte Zentren in [0..1], z.B. {0.25,0.5,0.75} oder {1/3,2/3}
-- icon_px_w: feste Pixelbreite pro Icon-Zelle
-- place_px_w: nutzbare Breite (Dialogbreite - linkes/rechtes Padding)
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
		row:add(icon_cells[i]) -- hat feste Breite (fixed_cell)
	end
	row:add(spacer_exact(S[n + 1] or 0))
	return row
end

function Base.choice(opts)
	local th = W.merge_theme(W.theme, opts.theme or {})
	local s = awful.screen.focused()

	-- feste Dialoggröße
	local DIALOG_W = th.dialog_w or 560
	local DIALOG_H = th.dialog_h or 360

	-- Höhen: 1/5 – 3/5 – 1/5
	local H_HEADER = math.floor(DIALOG_H / 5)
	local H_BODY = math.floor(DIALOG_H * 3 / 5)
	local H_FOOT = H_HEADER

	-- Icon-Größe (Quadrat) + sichtbare Zellbreite
	local ICON_SIZE = math.floor(DIALOG_H / 5)
	th.icon_size = ICON_SIZE
	local ICON_CELL_W = ICON_SIZE + 24 -- entspricht mk_icon (size + 24)

	-- Backdrop
	local backdrop = wibox({
		screen = s,
		visible = true,
		ontop = true,
		type = "splash",
		bg = th.backdrop or "#00000066",
	})
	backdrop:geometry(s.geometry)

	-- Header
	local header_area = W.mk_header(opts.title or "", th)

	-- Actions → Icon-Cells
	local actions = opts.actions or {}
	local n = #actions
	local built_cells = {}
	local close_ref = function() end

	for i = 1, n do
		local a = actions[i]
		local iconw = W.mk_icon({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = ICON_SIZE,
			label = a.label,
			on_press = function()
				if a.on_press then
					a.on_press(close_ref)
				end
			end,
			th = th,
		})
		built_cells[i] = fixed_cell(iconw, ICON_CELL_W)
	end

	-- nutzbare Breite im Body (Außenabstände abziehen)
	local PAD_H = th.pad_h or 16
	local place_w = DIALOG_W - 2 * PAD_H

	-- Ziel-Zentren
	local targets
	if n >= 3 then
		targets = { 0.25, 0.50, 0.75 } -- Power: ¼ / ½ / ¾
		while #built_cells > 3 do
			table.remove(built_cells)
		end
	elseif n == 2 then
		targets = { 1 / 3, 2 / 3 } -- Logout: ⅓ / ⅔
	elseif n == 1 then
		targets = { 0.5 } -- Ein Icon: Mitte
	else
		targets = {}
	end

	-- Reihe bauen (ohne ratio, nur mit fixen Spacern)
	local icons_row
	if #targets > 0 then
		icons_row = build_precise_fixed_row(built_cells, targets, ICON_CELL_W, place_w)
	else
		icons_row = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end

	-- Body
	local body_with_margins = wibox.widget({
		icons_row,
		left = PAD_H,
		right = PAD_H,
		top = th.pad_v or 14,
		bottom = th.pad_v or 14,
		widget = wibox.container.margin,
	})

	local body_centered = wibox.widget({
		body_with_margins,
		halign = "center",
		valign = "center",
		widget = wibox.container.place,
	})

	local body_area = wibox.widget({
		body_centered,
		bg = th.body_bg or "#DDEEFF",
		fg = th.body_fg or "#000000",
		widget = wibox.container.background,
	})

	-- Footer / Cancel
	local popup
	local function close()
		if popup and popup.visible then
			popup.visible = false
		end
		if backdrop and backdrop.visible then
			backdrop.visible = false
		end
	end
	close_ref = close

	th._computed_footer_h = H_FOOT
	th._computed_dialog_w = DIALOG_W
	local footer_area = W.mk_footer("Cancel", close, th)

	-- Fixhöhen
	local header_fixed = wibox.widget({
		header_area,
		strategy = "exact",
		height = H_HEADER,
		widget = wibox.container.constraint,
	})
	local body_fixed = wibox.widget({
		body_area,
		strategy = "exact",
		height = H_BODY,
		widget = wibox.container.constraint,
	})
	local footer_fixed = wibox.widget({
		footer_area,
		strategy = "exact",
		height = H_FOOT,
		widget = wibox.container.constraint,
	})

	-- Popup (Rundung wie gehabt)
	local rounded_block = wibox.widget({
		{
			header_fixed,
			body_fixed,
			footer_fixed,
			layout = wibox.layout.fixed.vertical,
		},
		bg = th.dialog_bg or "#00000000",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, th.radius or 12)
		end,
		shape_clip = true,
		widget = wibox.container.background,
	})

	local popup_widget = wibox.widget({
		{
			rounded_block,
			strategy = "exact",
			width = DIALOG_W,
			height = DIALOG_H,
			widget = wibox.container.constraint,
		},
		widget = wibox.container.background,
	})

	popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000",
		placement = awful.placement.centered,
		widget = popup_widget,
	})

	popup.visible = true
	awful.placement.centered(popup, { honor_workarea = true })

	-- ESC + Klick schließt
	backdrop:buttons(awful.button({}, 1, close))
	awful
		.keygrabber({
			mask_modkeys = true,
			stop_key = "Escape",
			stop_event = "release",
			keybindings = {
				{
					{},
					"Escape",
					function()
						close()
					end,
				},
			},
		})
		:start()

	return { close = close, popup = popup, backdrop = backdrop }
end

return Base
