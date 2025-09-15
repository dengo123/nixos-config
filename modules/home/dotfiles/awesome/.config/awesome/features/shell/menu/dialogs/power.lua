local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local Modal = require("features.shell.menu.dialogs.modal")

local M = {}

local function btn(label, theme, on_click)
	return wibox.widget({
		{
			{
				text = label,
				align = "center",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			margins = 10,
			widget = wibox.container.margin,
		},
		bg = (theme and theme.btn_bg) or "#ECECEC",
		fg = (theme and theme.btn_fg) or "#000000",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, theme and theme.radius or 6)
		end,
		widget = wibox.container.background,
		buttons = gears.table.join(awful.button({}, 1, function()
			if on_click then
				on_click()
			end
		end)),
	})
end

function M.open(theme)
	local close_modal

	local row = wibox.widget({
		btn("Power off", theme, function()
			if close_modal then
				close_modal()
			end
			if theme and theme.on_poweroff then
				theme.on_poweroff()
			else
				awful.spawn.with_shell("systemctl poweroff")
			end
		end),
		btn("Reboot", theme, function()
			if close_modal then
				close_modal()
			end
			if theme and theme.on_reboot then
				theme.on_reboot()
			else
				awful.spawn.with_shell("systemctl reboot")
			end
		end),
		btn("Hibernate", theme, function()
			if close_modal then
				close_modal()
			end
			if theme and theme.on_hibernate then
				theme.on_hibernate()
			else
				awful.spawn.with_shell("systemctl hibernate")
			end
		end),
		spacing = 12,
		layout = wibox.layout.fixed.horizontal,
	})

	close_modal = Modal.show({
		content = row,
		width = 420,
		height = 120,
		bg = theme and theme.bg,
		fg = theme and theme.fg,
		backdrop = theme and theme.backdrop,
		radius = theme and theme.radius,
	})
end

return M
