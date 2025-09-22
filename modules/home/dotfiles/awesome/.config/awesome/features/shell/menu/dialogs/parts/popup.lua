-- features/shell/menu/dialogs/parts/popup.lua
-- Theme-neutraler Popup-Wrapper mit robuster ESC-Handling (OO + Legacy Keygrabber)

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

local function nz(x, fallback)
	return x == nil and fallback or x
end

function Popup.show(form_widget, th, opts)
	opts = opts or {}
	th = th or {}
	local s = awful.screen.focused()

	local W = assert(opts.width, "popup.show: width required")
	local H = assert(opts.height, "popup.show: height required")

	local close_on_escape = nz(opts.close_on_escape, true)
	local close_on_backdrop = nz(opts.close_on_backdrop, false)

	-- Backdrop: blockt Klicks, schließt standardmäßig NICHT
	local backdrop = wibox({
		screen = s,
		visible = true,
		ontop = true,
		type = "splash",
		bg = nz(th.backdrop, "#00000000"),
	})
	backdrop:geometry(s.geometry)
	backdrop.input_passthrough = false

	if close_on_backdrop then
		backdrop:buttons(gears.table.join(
			awful.button({}, 1, function() end) -- wird von close() unten überschrieben; hier genügt Block+Schließen
		))
	else
		-- NO-OP-Handler zum Konsumieren von Klicks (blockt Clients/Menu darunter)
		backdrop:buttons(
			gears.table.join(
				awful.button({}, 1, function() end),
				awful.button({}, 2, function() end),
				awful.button({}, 3, function() end)
			)
		)
	end

	-- Innen-Container (neutral)
	local border_block = wibox.widget({
		form_widget,
		bg = nz(th.dialog_bg, "#00000000"),
		shape = function(cr, w, h)
			local r = tonumber(th.dialog_radius) or 0
			if r > 0 then
				gears.shape.rounded_rect(cr, w, h, r)
			else
				gears.shape.rectangle(cr, w, h)
			end
		end,
		shape_clip = true,
		shape_border_width = tonumber(th.dialog_border_width) or 0,
		shape_border_color = nz(th.dialog_border, "#00000000"),
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
	local kg -- kann OO-Objekt ODER Legacy-ID sein
	local function stop_grabber()
		if kg then
			if type(kg) == "table" and kg.stop then
				pcall(function()
					kg:stop()
				end) -- OO-API
			else
				pcall(function()
					awful.keygrabber.stop(kg)
				end) -- Legacy
			end
			kg = nil
		end
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

	-- Falls Backdrop-Schließen aktiv: echten Close-Handler setzen
	if close_on_backdrop then
		backdrop:buttons(
			gears.table.join(awful.button({}, 1, close), awful.button({}, 2, close), awful.button({}, 3, close))
		)
	end

	-- ESC schließt (robust für beide APIs + Masking von Lock-Keys)
	if close_on_escape then
		-- Versuche OO-API
		local ok, obj = pcall(function()
			return awful.keygrabber({
				mask_modkeys = true, -- NumLock/CapsLock etc. maskieren
				stop_key = "Escape",
				stop_event = "release",
				keybindings = {
					{ {}, "Escape", close }, -- auf Key-press schließen; stop auf release
				},
			})
		end)

		if ok and obj and obj.start then
			kg = obj
			kg:start()
		else
			-- Fallback: Legacy-API
			kg = awful.keygrabber.run(function(_, key, event)
				-- Maskiere Lock-Keys: wir reagieren nur auf Escape-Release
				if key == "Escape" and event == "release" then
					close()
				end
			end)
		end
	end

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
