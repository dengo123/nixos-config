-- ~/.config/awesome/features/shell/widgets/start.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

-- opts: { menu=?, label=?, icon=?, bg=?, bg_hover=?, fg=?, shape=?, margin? }
function M.build(s, opts)
	opts = opts or {}
	local menu = opts.menu -- erwartest du aus shell.init
	local label = opts.label or "Start"
	local icon = opts.icon or beautiful.awesome_icon
	local bg = opts.bg or (beautiful.bg_minimize or "#222222")
	local bg_hover = opts.bg_hover or (beautiful.bg_focus or "#444444")
	local fg = opts.fg or (beautiful.fg_normal or "#ffffff")
	local shape = opts.shape or gears.shape.rounded_rect
	local margin = opts.margin or { left = 8, right = 10, top = 4, bottom = 4 }

	local content = wibox.widget({
		{
			{
				{
					image = icon,
					resize = true,
					widget = wibox.widget.imagebox,
				},
				{
					text = label,
					widget = wibox.widget.textbox,
				},
				spacing = 6,
				layout = wibox.layout.fixed.horizontal,
			},
			left = margin.left,
			right = margin.right,
			top = margin.top,
			bottom = margin.bottom,
			widget = wibox.container.margin,
		},
		fg = fg,
		widget = wibox.container.background,
	})

	local btn = wibox.widget({
		content,
		bg = bg,
		shape = shape,
		widget = wibox.container.background,
	})

	-- Hover-Effekt
	btn:connect_signal("mouse::enter", function()
		btn.bg = bg_hover
	end)
	btn:connect_signal("mouse::leave", function()
		btn.bg = bg
	end)

	-- Klick → Menü unten links öffnen/umschalten
	btn:buttons(gears.table.join(awful.button({}, 1, function()
		if not menu then
			return
		end
		local scrg = (s or awful.screen.focused()).geometry
		-- unten links des Screens (y leicht ins Panel, damit’s sicher sichtbar ist)
		menu:toggle({ coords = { x = scrg.x, y = scrg.y + scrg.height - 1 } })
	end)))

	return btn
end

return M
