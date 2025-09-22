-- features/shell/menu/dialogs/parts/popup.lua
-- Theme-neutraler Popup-Wrapper:
-- - Kein festes Styling (alles über 'th')
-- - Lifecycle / ESC / Backdrop sauber gekapselt
-- - Backdrop blockt Klicks; schließt nur bei opts.close_on_backdrop=true

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup, _open = {}, {}

function Popup.close_all()
	for i = #_open, 1, -1 do
		local h = _open[i]
		if h and h.close then
			h.close(true)
		end
		_open[i] = nil
	end
end

local function nz(x, fb)
	return x == nil and fb or x
end

function Popup.show(form_widget, th, opts)
	opts, th = (opts or {}), (th or {})
	local s = awful.screen.focused()

	local W = assert(opts.width, "popup.show: width required")
	local H = assert(opts.height, "popup.show: height required")

	local close_on_escape = nz(opts.close_on_escape, true)
	local close_on_backdrop = nz(opts.close_on_backdrop, false)

	-- Backdrop (blockt Klicks, schließt NICHT standardmäßig)
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
		backdrop:buttons(gears.table.join(awful.button({}, 1, function()
			-- tatsächliches Schließen macht die lokale close()-Funktion weiter unten
		end)))
	else
		-- Klicks blocken, ohne zu schließen
		backdrop:buttons(
			gears.table.join(
				awful.button({}, 1, function() end),
				awful.button({}, 2, function() end),
				awful.button({}, 3, function() end)
			)
		)
	end

	-- Innencontainer (Optik via Theme)
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
		{ border_block, strategy = "exact", width = W, height = H, widget = wibox.container.constraint },
		widget = wibox.container.margin,
	})

	local popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = "#00000000", -- transparent
		placement = awful.placement.centered,
		widget = popup_widget,
	})

	--------------------------------------------------------------------------
	-- Robuster Lifecycle: lokale 'close' + Upvalues, ESC auf key *press*
	--------------------------------------------------------------------------
	local esc_grabber_id = nil

	local function stop_esc_grabber()
		if esc_grabber_id then
			pcall(function()
				awful.keygrabber.stop(esc_grabber_id)
			end)
			esc_grabber_id = nil
		end
	end

	local function remove_from_open()
		for i = #_open, 1, -1 do
			if _open[i] and _open[i].popup == popup then
				table.remove(_open, i)
				break
			end
		end
	end

	-- Vorwärtsdeklaration, damit Handler die *lokale* close fangen
	local close

	local function really_close()
		stop_esc_grabber()
		if popup then
			popup.visible = false
		end
		if backdrop then
			backdrop.visible = false
		end
		if popup then
			popup.widget = nil
		end
		remove_from_open()
		popup, backdrop = nil, nil
	end

	close = function() -- lokale Close-Funktion (idempotent)
		really_close()
	end

	-- Backdrop-Klick schließt nur bei Opt-in
	if close_on_backdrop then
		backdrop:buttons(gears.table.join(awful.button({}, 1, function()
			close()
		end)))
	end

	-- ESC-Grabber: wie im funktionierenden Menü → Legacy-API, auf PRESS reagieren
	if close_on_escape then
		esc_grabber_id = awful.keygrabber.run(function(_, key, event)
			if event == "release" then
				return
			end
			if key == "Escape" then
				close()
			end
		end)
	end

	popup.visible = true
	awful.placement.centered(popup, { honor_workarea = true })

	-- Guards: mit *lokalen* Upvalues auf close
	local on_tag_selected = function()
		close()
	end
	local on_screen_removed = function()
		close()
	end
	local on_screen_added = function()
		close()
	end
	local on_primary_changed = function()
		close()
	end

	tag.connect_signal("property::selected", on_tag_selected)
	screen.connect_signal("removed", on_screen_removed)
	screen.connect_signal("added", on_screen_added)
	screen.connect_signal("primary_changed", on_primary_changed)

	local handle = { close = close, popup = popup, backdrop = backdrop }
	table.insert(_open, handle)
	return handle
end

return Popup
