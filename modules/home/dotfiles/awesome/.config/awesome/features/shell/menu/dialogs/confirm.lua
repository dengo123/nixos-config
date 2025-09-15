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
			margins = 8,
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

function M.open(args)
	args = args or {}
	local theme = args.theme or {}
	local close_modal

	local text = wibox.widget({
		text = args.text or "Are you sure?",
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local buttons = wibox.widget({
		nil,
		{
			btn(args.ok_label or "Okay", theme, function()
				if close_modal then
					close_modal()
				end
				if args.on_ok then
					args.on_ok()
				end
			end),
			btn(args.cancel_label or "Cancel", theme, function()
				if close_modal then
					close_modal()
				end
				if args.on_cancel then
					args.on_cancel()
				end
			end),
			spacing = 10,
			layout = wibox.layout.fixed.horizontal,
		},
		expand = "outside",
		layout = wibox.layout.align.horizontal,
	})

	local col = wibox.widget({
		text,
		buttons,
		spacing = 14,
		layout = wibox.layout.fixed.vertical,
	})

	close_modal = Modal.show({
		content = col,
		width = 320,
		height = 140,
		bg = theme.bg,
		fg = theme.fg,
		backdrop = theme.backdrop,
		radius = theme.radius,
	})
end

function M.logoff(theme)
	M.open({
		text = "Are you sure?",
		ok_label = "Okay",
		cancel_label = "Cancel",
		on_ok = function()
			awful.spawn.with_shell("loginctl terminate-user $USER")
			-- oder: awesome.quit()
		end,
		theme = theme,
	})
end

return M
