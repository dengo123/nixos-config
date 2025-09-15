-- ~/.config/awesome/features/shell/menu/shared/dialogs/base.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local W = require("features.shell.menu.shared.dialogs.widgets")

local Base = {}

function Base.choice(opts)
	local th = W.merge_theme(W.theme, opts.theme or {})
	local s = awful.screen.focused()

	-- --- feste Dialoggröße --------------------------------------
	local DIALOG_W = th.dialog_w or 560
	local DIALOG_H = th.dialog_h or 360

	-- Höhen: 1/5 – 3/5 – 1/5
	local H_HEADER = math.floor(DIALOG_H / 5)
	local H_BODY = math.floor(DIALOG_H * 3 / 5)
	local H_FOOT = H_HEADER

	-- Icongröße: 1/5 × 1/5
	local ICON_SIZE = math.floor(DIALOG_H / 5)
	th.icon_size = ICON_SIZE

	-- Backdrop (halbtransparent)
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

	-- Actions → Icons
	local S = th.icons_spacing or 24
	local icons_row = wibox.widget({
		spacing = S,
		layout = wibox.layout.fixed.horizontal,
	})

	local close_ref = function() end
	for _, a in ipairs(opts.actions or {}) do
		icons_row:add(W.mk_icon({
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
		}))
	end

	local row_with_edges = wibox.widget({
		icons_row,
		left = S,
		right = S,
		top = th.pad_v or 14,
		bottom = th.pad_v or 14,
		widget = wibox.container.margin,
	})

	local body_centered = wibox.widget({
		row_with_edges,
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

	-- Footer
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
	-- diese beiden Werte braucht mk_footer für die exakten Cancel-Maße
	th._computed_footer_h = H_FOOT
	th._computed_dialog_w = DIALOG_W

	local footer_area = W.mk_footer("Cancel", close, th)

	-- Fixhöhen
	local header_fixed =
		wibox.widget({ header_area, strategy = "exact", height = H_HEADER, widget = wibox.container.constraint })
	local body_fixed =
		wibox.widget({ body_area, strategy = "exact", height = H_BODY, widget = wibox.container.constraint })
	local footer_fixed =
		wibox.widget({ footer_area, strategy = "exact", height = H_FOOT, widget = wibox.container.constraint })

	-- Runder Block
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

	-- Popup
	popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000",
		placement = awful.placement.centered,
		widget = wibox.widget({
			{
				rounded_block,
				strategy = "exact",
				width = DIALOG_W,
				height = DIALOG_H,
				widget = wibox.container.constraint,
			},
			widget = wibox.container.background,
		}),
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
