-- ~/.config/awesome/features/shell/menu/shared/dialogs/base.lua
-- Dialog logic: sizes/ratios, placement, backdrop, ESC handling.
-- Style from features/shell/menu/dialogs/parts/theme.lua
-- Widgets from features/shell/menu/dialogs/parts/widgets.lua

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local Theme = require("features.shell.menu.dialogs.parts.theme")

local Base = {}

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
	local DIALOG_W = th.dialog_w or 560
	local DIALOG_H = th.dialog_h or 360

	-- header/body/footer heights
	local HR = th.header_ratio or 0.18
	local FR = th.footer_ratio or 0.18
	local H_HEADER = math.floor(DIALOG_H * HR)
	local H_FOOT = math.floor(DIALOG_H * FR)
	local H_BODY = DIALOG_H - H_HEADER - H_FOOT

	-- icon size
	local base_side = math.min(DIALOG_W, DIALOG_H)
	local ICON_SIZE_RAW = math.floor(base_side * (th.icon_ratio or 0.20))
	local PAD_V = th.pad_v or 14
	local ICON_MAX = math.max(8, H_BODY - 2 * PAD_V)
	local ICON_SIZE = math.min(ICON_SIZE_RAW, ICON_MAX)
	th.icon_size = ICON_SIZE

	-- cell width
	local icon_pad = th.icon_pad or 6
	local cell_pad = th.icon_cell_pad or 6
	local cell_extra = th.icon_cell_extra_w or 12
	local ICON_CELL_W = ICON_SIZE + icon_pad * 2 + cell_pad * 2 + cell_extra

	-- backdrop
	local backdrop = wibox({
		screen = s,
		visible = true,
		ontop = true,
		type = "splash",
		bg = th.backdrop or "#00000066",
	})
	backdrop:geometry(s.geometry)

	-- header
	local header_area = W.mk_header(opts.title or "", th)

	-- actions -> icon cells
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

	-- usable width in body
	local PAD_H = th.pad_h or 16
	local place_w = DIALOG_W - 2 * PAD_H

	-- evenly spaced targets
	local targets = {}
	if n > 0 then
		for i = 1, n do
			targets[i] = (i - 0.5) / n
		end
	end

	-- row
	local icons_row
	if #targets > 0 then
		icons_row = build_precise_fixed_row(built_cells, targets, ICON_CELL_W, place_w)
	else
		icons_row = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end

	-- body
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
		bg = th.body_bg or "#00000000",
		fg = th.body_fg or "#FFFFFF",
		widget = wibox.container.background,
	})

	-- footer/cancel
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

	-- fixed heights
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

	-- popup mit Border innen (Gesamtgröße unverändert)
	local border_block = wibox.widget({
		{
			header_fixed,
			body_fixed,
			footer_fixed,
			layout = wibox.layout.fixed.vertical,
		},
		bg = th.dialog_bg or "#000000",
		shape = function(cr, w, h)
			if (th.dialog_radius or 0) > 0 then
				gears.shape.rounded_rect(cr, w, h, th.dialog_radius)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		shape_clip = true,
		shape_border_width = th.dialog_border_width or 1,
		shape_border_color = th.dialog_border or "#FFFFFF",
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
		margins = 2,
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
