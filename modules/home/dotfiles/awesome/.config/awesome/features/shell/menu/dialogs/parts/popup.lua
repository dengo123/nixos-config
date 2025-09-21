local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}
local _open = {}

function Popup.close_all()
	for i = #_open, 1, -1 do
		local h = _open[i]
		if h and h.close then
			h.close(true)
		end
		_open[i] = nil
	end
end

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

function Popup.show(form_widget, th, opts)
	opts = opts or {}
	th = th or {}
	local s = awful.screen.focused()

	local W = assert(opts.width, "popup.show: width required")
	local H = assert(opts.height, "popup.show: height required")

	-- Backdrop
	local backdrop = wibox({
		screen = s,
		visible = true,
		ontop = true,
		type = "splash",
		bg = pick(th.backdrop, "#00000066"),
	})
	backdrop:geometry(s.geometry)

	-- Border innen
	local border_block = wibox.widget({
		form_widget,
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
			width = W,
			height = H,
			widget = wibox.container.constraint,
		},
		widget = wibox.container.margin,
	})

	local popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000",
		placement = awful.placement.centered,
		widget = popup_widget,
	})

	-- Lifecycle
	local kg
	local function stop_grabber()
		if kg and kg.stop then
			pcall(function()
				kg:stop()
			end)
		end
		kg = nil
	end

	local function really_close()
		if popup then
			popup.visible = false
		end
		if backdrop then
			backdrop.visible = false
		end
		if popup then
			popup.widget = nil
		end
		stop_grabber()
		popup, backdrop = nil, nil
	end

	local function close()
		really_close() -- idempotent
	end

	-- ESC + Backdrop
	backdrop:buttons(gears.table.join(awful.button({}, 1, close)))
	kg = awful.keygrabber({
		mask_modkeys = true,
		stop_key = "Escape",
		stop_event = "release",
		keybindings = { {
			{},
			"Escape",
			function()
				close()
			end,
		} },
	})
	kg:start()

	popup.visible = true
	awful.placement.centered(popup, { honor_workarea = true })

	-- Guards
	tag.connect_signal("property::selected", close)
	screen.connect_signal("removed", close)
	screen.connect_signal("added", close)
	screen.connect_signal("primary_changed", close)

	local handle = { close = close, popup = popup, backdrop = backdrop }
	table.insert(_open, handle)
	return handle
end

return Popup
