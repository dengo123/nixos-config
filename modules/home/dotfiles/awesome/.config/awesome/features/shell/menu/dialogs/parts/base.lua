-- Dialog-Komposition: Größen/Verhältnisse, Backdrop, ESC, Border innen.
-- Baut Header/Footer-Container, nutzt atomare Widgets aus parts/widgets.lua.

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local Theme = require("features.shell.menu.dialogs.parts.theme")

local Base = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- keep a widget centered inside a fixed-width cell
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

-- spacer with exact pixel width
local function spacer_exact(px)
	return wibox.widget({
		wibox.widget({}),
		strategy = "exact",
		width = math.max(0, math.floor(px or 0)),
		widget = wibox.container.constraint,
	})
end

-- build row: [S1][I1][S2][I2]...[Sn][In][S(n+1)] using fixed.horizontal
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

function Base.choice(opts)
	opts = opts or {}

	local th = Theme.get(opts.theme or {})
	local s = awful.screen.focused()

	-- dialog size
	local DIALOG_W = pick(th.dialog_w, 560)
	local DIALOG_H = pick(th.dialog_h, 360)

	-- header/body/footer heights
	local HR = pick(th.header_ratio, 0.18)
	local FR = pick(th.footer_ratio, 0.18)
	local H_HEADER = math.floor(DIALOG_H * HR)
	local H_FOOT = math.floor(DIALOG_H * FR)
	local H_BODY = DIALOG_H - H_HEADER - H_FOOT

	-- icon size
	local base_side = math.min(DIALOG_W, DIALOG_H)
	local ICON_SIZE_RAW = math.floor(base_side * pick(th.icon_ratio, 0.20))
	local PAD_V = pick(th.pad_v, 14)
	local ICON_MAX = math.max(8, H_BODY - 2 * PAD_V)
	local ICON_SIZE = math.min(ICON_SIZE_RAW, ICON_MAX)
	th.icon_size = ICON_SIZE

	-- cell width
	local icon_pad = pick(th.icon_pad, 6)
	local cell_pad = pick(th.icon_cell_pad, 6)
	local cell_extra = pick(th.icon_cell_extra_w, 12)
	local ICON_CELL_W = ICON_SIZE + icon_pad * 2 + cell_pad * 2 + cell_extra

	-- backdrop
	local backdrop = wibox({
		screen = s,
		visible = true,
		ontop = true,
		type = "splash",
		bg = pick(th.backdrop, "#00000066"),
	})
	backdrop:geometry(s.geometry)

	-- =======================
	-- Header-Container bauen
	-- =======================
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

	-- actions -> icon cells
	local actions = opts.actions or {}
	local n = #actions
	local built_cells = {}
	local close_ref = function() end

	for i = 1, n do
		local a = actions[i]
		local iconw = W.mk_icon_button({
			icon = a.icon,
			emoji = a.emoji,
			emoji_font = a.emoji_font,
			size = ICON_SIZE,
			label = a.label,
			th = th,
		})

		local cell = fixed_cell(iconw, ICON_CELL_W)
		-- Klick auf den äußersten Wrapper der Zelle
		cell:buttons(gears.table.join(awful.button({}, 1, function()
			if a.on_press then
				a.on_press(close_ref)
			end
		end)))
		built_cells[i] = cell
	end

	-- usable width in body
	local PAD_H = pick(th.pad_h, 16)
	local place_w = DIALOG_W - 2 * PAD_H

	-- evenly spaced targets
	local targets = {}
	if n > 0 then
		for i = 1, n do
			targets[i] = (i - 0.5) / n
		end
	end

	local icons_row = (#targets > 0) and build_precise_fixed_row(built_cells, targets, ICON_CELL_W, place_w)
		or wibox.widget({ layout = wibox.layout.fixed.horizontal })

	-- body
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

	-- =======================
	-- Footer-Container bauen
	-- =======================
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

	local cancel_btn = W.mk_cancel_button((th.cancel_label or "Cancel"), close, th)

	-- optionale feste Breite via Ratio (z. B. 1/7 vom Dialog)
	local target_w = th.cancel_width or (DIALOG_W > 0 and math.floor(DIALOG_W / 7) or nil)
	if target_w then
		local wrapped = wibox.widget({
			cancel_btn,
			strategy = "exact",
			width = target_w,
			widget = wibox.container.constraint,
		})
		-- Buttons auf äußersten Wrapper legen (zusätzlich zum inneren)
		wrapped:buttons(gears.table.join(awful.button({}, 1, function()
			close()
		end)))
		cancel_btn = wrapped
	end

	local footer_right = wibox.widget({
		{
			cancel_btn,
			halign = "right",
			valign = "center",
			widget = wibox.container.place,
		},
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

	-- fixed heights
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

	-- popup mit Border innen (Gesamtgröße unverändert)
	local border_block = wibox.widget({
		{
			header_fixed,
			body_fixed,
			footer_fixed,
			layout = wibox.layout.fixed.vertical,
		},
		bg = pick(th.dialog_bg, "#111318"),
		shape = function(cr, w, h)
			local r = pick(th.dialog_radius, 12)
			if r > 0 then
				gears.shape.rounded_rect(cr, w, h, r)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		shape_clip = true,
		shape_border_width = pick(th.dialog_border_width, 1),
		shape_border_color = pick(th.dialog_border, "#3A6EA5"),
		widget = wibox.container.background,
	})

	local popup_widget = wibox.widget({
		{
			border_block,
			strategy = "exact",
			width = DIALOG_W,
			height = DIALOG_H,
			widget = wibox.container.constraint,
		},
		-- kein äußerer transparenter Rand mehr
		widget = wibox.container.margin,
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

	-- close on ESC and backdrop click
	backdrop:buttons(gears.table.join(awful.button({}, 1, close)))
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
