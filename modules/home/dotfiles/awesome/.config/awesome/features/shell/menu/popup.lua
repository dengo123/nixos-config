-- features/shell/menu/popup.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}

-- args = { header, cols, footer, theme, placement? }
function Popup.build(args)
	local header = args.header
	local cols = args.cols
	local footer = args.footer
	local t = args.theme or {}

	-- Root-Widget
	local root = wibox.widget({
		header,
		cols,
		footer,
		layout = wibox.layout.align.vertical,
	})

	-- Popup selbst
	local popup = awful.popup({
		widget = root,
		visible = false,
		ontop = true,
		shape = t.shape or gears.shape.rectangle,
		border_width = t.border_width or 1,
		border_color = t.border_color or "#000000",
		bg = t.bg or "#222222",
		fg = t.fg or "#ffffff",
		minimum_width = (t.col_left_w or 320) + (t.col_right_w or 220) + (t.cols_pad_l or 6) + (t.cols_pad_r or 6),
		minimum_height = t.total_height or 520,
		-- kein Placement hier, das übernimmt show()
	})

	-- optionale Custom-Placement-Funktion
	local place_fn = args.placement
		or function(p, s)
			local g = s.geometry
			p.x = g.x
			p.y = g.y + g.height - (t.total_height or 520)
		end

	-- Key- und Mousegrabber ---------------------------------------------------
	local keygrabber
	local mouse_running = false

	local function keygrabber_running(kg)
		if not kg then
			return false
		end
		return (kg.is_running == true) or (type(kg.is_running) == "function" and kg.is_running(kg))
	end

	local function start_keygrabber(api)
		if keygrabber_running(keygrabber) then
			return
		end
		keygrabber = awful.keygrabber({
			stop_event = "release",
			keypressed_callback = function(_, _, key)
				if key == "Escape" or key == "Super_L" or key == "Super_R" then
					api:hide()
				end
			end,
		})
		keygrabber:start()
	end

	local function stop_keygrabber()
		if keygrabber_running(keygrabber) then
			keygrabber:stop()
		end
	end

	local function start_mousegrabber(api)
		if mouse_running then
			return
		end
		mouse_running = true
		awful.mousegrabber.run(function(m)
			if m.buttons[1] then
				local gx, gy, gw, gh = popup.x, popup.y, popup.width, popup.height
				local inside = m.x >= gx and m.x <= (gx + gw) and m.y >= gy and m.y <= (gy + gh)
				if not inside then
					api:hide()
					return false -- Klick geht weiter an Button
				end
			end
			return true
		end, "left_ptr")
	end

	local function stop_mousegrabber()
		if mouse_running then
			awful.mousegrabber.stop()
			mouse_running = false
		end
	end
	---------------------------------------------------------------------------

	-- Öffentliche API
	local api = {}

	-- opts: { coords = {x=,y=}, screen = <screen> }
	function api:show(opts)
		local s = (opts and opts.screen) or mouse.screen or awful.screen.focused()
		popup.screen = s
		place_fn(popup, s)
		if opts and opts.coords then
			popup.x = opts.coords.x or popup.x
			popup.y = opts.coords.y or popup.y
		end
		popup.visible = true
		start_keygrabber(api)
		start_mousegrabber(api)
	end

	function api:hide()
		popup.visible = false
		stop_keygrabber()
		stop_mousegrabber()
	end

	function api:toggle(opts)
		if popup.visible then
			self:hide()
		else
			self:show(opts)
		end
	end

	-- Safety: falls Popup anderswo unsichtbar wird, Grabber stoppen
	popup:connect_signal("property::visible", function()
		if not popup.visible then
			stop_keygrabber()
			stop_mousegrabber()
		end
	end)

	return api
end

return Popup
