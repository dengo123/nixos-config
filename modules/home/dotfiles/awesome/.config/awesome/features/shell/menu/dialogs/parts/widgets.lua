-- ~/.config/awesome/features/shell/menu/dialogs/widgets.lua
-- UI-Bausteine für Dialogs (Header, Footer, Icon).
-- Erwartet Theme-Tabelle (th), die von außen übergeben wird.
-- Theme-Werte selbst kommen aus features/shell/menu/dialogs/theme.lua.

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local Theme = require("features.shell.menu.dialogs.parts.theme")

local W = {}

-- ===== Header ===============================================================
function W.mk_header(title, th)
	return wibox.widget({
		{
			{
				markup = "<b>" .. (title or "") .. "</b>",
				align = "left",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = th.pad_h,
			right = th.pad_h,
			widget = wibox.container.margin,
		},
		bg = th.header_bg,
		fg = th.header_fg,
		widget = wibox.container.background,
	})
end

-- ===== Footer mit Cancel rechts, Hover = blauer Rand ========================
function W.mk_footer(cancel_label, on_cancel, th)
	local inner = wibox.widget({
		{
			{
				text = cancel_label or "Cancel",
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = 10,
			right = 10,
			top = 4,
			bottom = 4,
			widget = wibox.container.margin,
		},
		bg = th.cancel_bg or "#ECECEC",
		fg = th.cancel_fg or "#000000",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, 2)
		end,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})

	local footer_h = th._computed_footer_h or 0
	local dialog_w = th._computed_dialog_w or 0
	local inner_h = math.max(0, footer_h - 16)
	local target_h = inner_h > 0 and math.floor(inner_h * 2 / 5) or nil
	local target_w = dialog_w > 0 and math.floor(dialog_w / 5) or nil

	local cancel_btn = wibox.widget({
		inner,
		strategy = "exact",
		width = target_w,
		height = target_h,
		widget = wibox.container.constraint,
	})

	cancel_btn:connect_signal("mouse::enter", function()
		inner.bg = th.cancel_hover_bg or th.cancel_bg or "#ECECEC"
		inner.shape_border_width = 2
		inner.shape_border_color = th.cancel_hover_border or "#2B5B88"
	end)
	cancel_btn:connect_signal("mouse::leave", function()
		inner.bg = th.cancel_bg or "#ECECEC"
		inner.shape_border_width = 0
		inner.shape_border_color = "#00000000"
	end)
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		if on_cancel then
			on_cancel()
		end
	end)))

	local right_cell = wibox.widget({
		{
			cancel_btn,
			halign = "right",
			valign = "center",
			widget = wibox.container.place,
		},
		right = th.pad_h or 16,
		widget = wibox.container.margin,
	})

	local row = wibox.widget({
		nil,
		nil,
		right_cell,
		layout = wibox.layout.align.horizontal,
		expand = "none",
	})

	return wibox.widget({
		{ row, top = 8, bottom = 8, widget = wibox.container.margin },
		bg = th.footer_bg or "#2B5B88",
		fg = th.footer_fg or "#FFFFFF",
		widget = wibox.container.background,
	})
end

-- ===== Nacktes Icon (quadratisch) + optionales Label ========================
-- args: { size, icon|emoji, emoji_font?, label?, on_press?, th? }
function W.mk_icon(args)
	local th = args.th or Theme.defaults
	local size = args.size or 64

	-- Icon selbst (unverändert)
	local icon_widget
	if args.icon and type(args.icon) == "string" and args.icon:match("^/") then
		icon_widget = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	else
		icon_widget = wibox.widget({
			markup = string.format(
				"<span font='%s'>%s</span>",
				args.emoji_font or ("sans " .. math.floor(size * 0.66)),
				args.emoji or "⏻"
			),
			align = "center",
			valign = "center",
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.textbox,
		})
	end

	-- NEU: Label größer, weiß, mit mehr Abstand
	local label_widget
	if args.label and #args.label > 0 then
		label_widget = wibox.widget({
			{
				markup = string.format("<span font='sans 14' color='#FFFFFF'>%s</span>", args.label),
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			top = 12, -- vorher 6 → mehr Abstand zum Icon
			widget = wibox.container.margin,
		})
	end

	local stack = wibox.widget({
		icon_widget,
		label_widget,
		layout = wibox.layout.fixed.vertical,
	})

	-- Rest (Hoverfläche, Klicks, Constraints) bleibt wie gehabt …
	local clickable_bg = wibox.widget({
		{
			stack,
			left = 6,
			right = 6,
			top = 6,
			bottom = 6,
			widget = wibox.container.margin,
		},
		bg = "#00000000",
		shape = gears.shape.rounded_rect,
		widget = wibox.container.background,
	})

	local base_h = size + 12
	local total_h = base_h
	if label_widget then
		total_h = total_h + 12 + 18 -- an top=12 angepasst
	end

	local clickable = wibox.widget({
		clickable_bg,
		strategy = "exact",
		width = size + 24,
		height = total_h,
		widget = wibox.container.constraint,
	})

	if args.on_press then
		clickable:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	clickable:connect_signal("mouse::enter", function()
		clickable_bg.bg = th.icon_hover_bg or "#FFFFFF20"
		clickable_bg.shape_border_width = 2
		clickable_bg.shape_border_color = th.icon_hover_border or "#2B5B88"
	end)
	clickable:connect_signal("mouse::leave", function()
		clickable_bg.bg = "#00000000"
		clickable_bg.shape_border_width = 0
		clickable_bg.shape_border_color = "#00000000"
	end)

	return clickable
end

return W
