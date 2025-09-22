-- features/shell/menu/dialogs/parts/base.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local H = require("features.shell/menu/dialogs/parts/helpers") -- <—
local Popup = require("features.shell.menu.dialogs.parts.popup")

local Base = {}

function Base.choice(opts)
	opts = opts or {}
	local th = opts.theme or {}

	local DIALOG_W = H.pick(th.dialog_w, 560)
	local DIALOG_H = H.pick(th.dialog_h, 360)

	local geom = H.compute_icon_metrics(th, DIALOG_W, DIALOG_H)

	-- Header
	local header_content = W.mk_header_content(opts.title or "", th)
	local header_container = wibox.widget({
		{
			header_content,
			left = H.pick(th.header_pad_h, th.pad_h, 12),
			right = H.pick(th.header_pad_h, th.pad_h, 12),
			widget = wibox.container.margin,
		},
		bg = H.pick(th.header_bg, "#235CDB"),
		fg = H.pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Actions → Icon-Zellen
	local actions = opts.actions or {}
	local cells = {}
	local close_ref = function() end

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
	local icons_row = (#cells > 0) and H.build_even_row(cells, targets, geom.icon_cell_w, geom.place_w)
		or wibox.widget({ layout = wibox.layout.fixed.horizontal })

	local body_with_margins = wibox.widget({
		icons_row,
		left = geom.pad_h,
		right = geom.pad_h,
		top = geom.pad_v,
		bottom = geom.pad_v,
		widget = wibox.container.margin,
	})
	local body_container = wibox.widget({
		{ body_with_margins, halign = "center", valign = "center", widget = wibox.container.place },
		bg = H.pick(th.body_bg, "#00000000"),
		fg = H.pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Footer
	local cancel_btn = W.mk_cancel_button(th.cancel_label or "Cancel", function() end, th)
	local target_w = th.cancel_width or (DIALOG_W > 0 and math.floor(DIALOG_W / 7) or nil)
	if target_w then
		cancel_btn =
			wibox.widget({ cancel_btn, strategy = "exact", width = target_w, widget = wibox.container.constraint })
	end
	local footer_right = wibox.widget({
		{ cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
		right = H.pick(th.footer_pad_h, th.pad_h, 12),
		widget = wibox.container.margin,
	})
	local footer_container = wibox.widget({
		{
			footer_right,
			top = H.pick(th.footer_pad_v, 8),
			bottom = H.pick(th.footer_pad_v, 8),
			widget = wibox.container.margin,
		},
		bg = H.pick(th.footer_bg, "#235CDB"),
		fg = H.pick(th.footer_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Fixhöhen
	local seg = geom.seg
	local header_fixed = wibox.widget({
		header_container,
		strategy = "exact",
		height = seg.header,
		widget = wibox.container.constraint,
	})
	local body_fixed =
		wibox.widget({ body_container, strategy = "exact", height = seg.body, widget = wibox.container.constraint })
	local footer_fixed = wibox.widget({
		footer_container,
		strategy = "exact",
		height = seg.footer,
		widget = wibox.container.constraint,
	})

	local form_widget = wibox.widget({ header_fixed, body_fixed, footer_fixed, layout = wibox.layout.fixed.vertical })

	local handle = Popup.show(form_widget, th, {
		width = DIALOG_W,
		height = DIALOG_H,
		close_on_escape = true,
		close_on_backdrop = false,
	})

	close_ref = handle.close
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		handle.close()
	end)))
	return handle
end

return Base
