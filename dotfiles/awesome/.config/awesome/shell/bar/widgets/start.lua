-- ~/.config/awesome/shell/bar/widgets/start.lua
local awful = require("awful")
local gears = require("gears")
local gfs = require("gears.filesystem")
local gsurface = require("gears.surface")
local wibox = require("wibox")

local M = {}

local function resolve_screen(opts)
	return (opts and opts.screen) or (mouse and mouse.screen) or nil
end

function M.build(opts)
	opts = opts or {}

	local screen = resolve_screen(opts)
	local theme = opts.theme or {}
	local bar_height = tonumber(opts.bar_height)

	local on_left_click = opts.on_left_click
	local on_right_click = opts.on_right_click

	local bg = theme.bg
	local bg_hover = theme.bg_hover
	local fg = theme.fg
	local shape = theme.shape
	local margin = theme.margin or { left = 0, right = 0, top = 0, bottom = 0 }

	local width = math.floor((tonumber(theme.width_factor) or 4) * (bar_height or 1))

	local icon_path = theme.icon
	if type(icon_path) == "string" and not icon_path:match("^/") then
		icon_path = gfs.get_configuration_dir() .. icon_path
	end

	local icon_surface = (type(icon_path) == "string") and gsurface.load_uncached(icon_path) or icon_path

	local inner_height = (bar_height or 0) - ((margin.top or 0) + (margin.bottom or 0))
	local icon_px = math.min(theme.icon_size or inner_height, inner_height)

	local icon_widget = wibox.widget({
		image = icon_surface,
		resize = true,
		upscale = true,
		downscale = true,
		forced_width = icon_px,
		forced_height = icon_px,
		widget = wibox.widget.imagebox,
	})

	local label_widget = wibox.widget({
		text = theme.label,
		font = theme.font,
		widget = wibox.widget.textbox,
	})

	local row_inner = wibox.widget({
		icon_widget,
		label_widget,
		spacing = tonumber(theme.spacing),
		layout = wibox.layout.fixed.horizontal,
	})

	local row = wibox.widget({
		{
			row_inner,
			left = margin.left or 0,
			right = margin.right or 0,
			top = margin.top or 0,
			bottom = margin.bottom or 0,
			widget = wibox.container.margin,
		},
		fg = fg,
		widget = wibox.container.background,
	})

	local btn = wibox.widget({
		row,
		bg = bg,
		shape = shape,
		forced_width = width,
		forced_height = theme.fixed_height and bar_height or nil,
		widget = wibox.container.background,
	})

	btn:connect_signal("mouse::enter", function()
		btn.bg = bg_hover
	end)

	btn:connect_signal("mouse::leave", function()
		btn.bg = bg
	end)

	btn:buttons(gears.table.join(
		awful.button({}, 1, function()
			if type(on_left_click) == "function" then
				on_left_click({
					screen = screen,
					widget = btn,
				})
			end
		end),
		awful.button({}, 3, function()
			if type(on_right_click) == "function" then
				on_right_click({
					screen = screen,
					widget = btn,
				})
			end
		end)
	))

	return btn
end

return M
