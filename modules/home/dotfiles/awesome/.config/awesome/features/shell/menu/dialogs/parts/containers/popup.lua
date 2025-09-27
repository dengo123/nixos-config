-- features/shell/menu/dialogs/parts/popup.lua
-- Theme-neutraler Popup-Wrapper:
-- - Kein festes Styling (alles über 'th')
-- - Lifecycle / ESC / Backdrop sauber gekapselt
-- - Backdrop blockt Klicks; schließt nur bei opts.close_on_backdrop=true
-- - show_root-Modi:
--     false/nil: normal (nichts verstecken)
--     "with_bars": Clients ausblenden, Bars sichtbar lassen
--     "full" oder true: Clients und Bars ausblenden (klassischer Power-Screen)

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup, _open = {}, {}

-- --------------------------------------------------------------------
-- Utils
-- --------------------------------------------------------------------
local function nz(x, fb)
	return x == nil and fb or x
end

local function set_clients_visible(scr, on)
	local ok, clients = pcall(function()
		return client.get(scr)
	end)
	if ok and clients then
		for _, c in ipairs(clients) do
			c.hidden = not on
		end
	end
end

local function set_bars_visible(scr, on)
	-- Erwartet screen.bars = { mybar1, mybar2, ... } o.ä. (optional)
	for _, bar in pairs(scr.bars or {}) do
		if bar and bar.visible ~= nil then
			bar.visible = on
		end
	end
end

-- --------------------------------------------------------------------
-- Public API
-- --------------------------------------------------------------------
function Popup.close_all()
	for i = #_open, 1, -1 do
		local h = _open[i]
		if h and h.close then
			h.close(true)
		end
		_open[i] = nil
	end
end

function Popup.show(form_widget, th, opts)
	opts, th = (opts or {}), (th or {})
	local s = awful.screen.focused()

	local W = assert(opts.width, "popup.show: width required")
	local H = assert(opts.height, "popup.show: height required")

	local use_backdrop = (opts.use_backdrop ~= false) -- default = true
	local close_on_escape = nz(opts.close_on_escape, true)
	local close_on_backdrop = nz(opts.close_on_backdrop, false)
	local placement_fn = opts.placement or awful.placement.centered

	-- Root-Modus: Clients/Bars manipulieren
	local show_root_mode = opts.show_root or false
	if show_root_mode == true then
		show_root_mode = "full"
	end
	local hide_clients = (show_root_mode == "with_bars") or (show_root_mode == "full")
	local hide_bars = (show_root_mode == "full")

	-- Backdrop (optional)
	local backdrop = nil
	if use_backdrop then
		backdrop = wibox({
			screen = s,
			visible = true,
			ontop = true,
			type = "splash",
			bg = nz(th.backdrop, "#00000000"),
		})
		backdrop.input_passthrough = false

		if show_root_mode == "with_bars" then
			-- nur Workarea abdunkeln → Bars bleiben sichtbar
			local wa = s.workarea
			backdrop:geometry({ x = wa.x, y = wa.y, width = wa.width, height = wa.height })
		else
			backdrop:geometry(s.geometry)
		end

		if close_on_backdrop then
			backdrop:buttons(gears.table.join(awful.button({}, 1, function()
				close()
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
		placement = placement_fn,
		widget = popup_widget,
	})

	----------------------------------------------------------------------
	-- Robuster Lifecycle
	----------------------------------------------------------------------
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

	local close -- forward-declared

	local function really_close()
		stop_esc_grabber()

		if popup then
			popup.visible = false
		end
		if backdrop then
			backdrop.visible = false
		end

		-- Root-Restore
		if hide_clients then
			set_clients_visible(s, true)
		end
		if hide_bars then
			set_bars_visible(s, true)
		end

		if popup then
			popup.widget = nil
		end
		remove_from_open()
		popup, backdrop = nil, nil
	end

	close = function()
		really_close()
	end

	-- Root hide anwenden
	if hide_clients then
		set_clients_visible(s, false)
	end
	if hide_bars then
		set_bars_visible(s, false)
	end

	-- ESC-Grabber
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
	placement_fn(popup, { honor_workarea = true })

	-- Guards
	local function on_tag_selected()
		close()
	end
	local function on_screen_removed()
		close()
	end
	local function on_screen_added()
		close()
	end
	local function on_primary_changed()
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
