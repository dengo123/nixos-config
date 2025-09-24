-- ~/.config/awesome/features/shell/menu/parts/popup.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}

local function is_widget(x)
	return type(x) == "table"
		and (
			(type(x.emit_signal) == "function" and x.connect_signal ~= nil)
			or (x._private ~= nil and type(x.draw) == "function")
		)
end

-- Wrap ein Root-Widget und stelle NUR Popup-Cycle + Overlay bereit.
-- opts = { on_hide?, scrim_bg? (default "#00000099") }
function Popup.wrap(root_widget, opts)
	assert(is_widget(root_widget), "Popup.wrap: root_widget must be a widget")
	opts = opts or {}
	local on_hide = opts.on_hide
	local scrim_bg = opts.scrim_bg or "#00000099"

	-- Overlay-Slot (zentriert), aber KEINE Größen/Border-Logik hier!
	local overlay_slot = wibox.widget({ layout = wibox.layout.fixed.vertical })
	local overlay_root = wibox.widget({
		{
			{ overlay_slot, widget = wibox.container.place, halign = "center", valign = "center" },
			widget = wibox.container.background,
			bg = scrim_bg,
		},
		visible = false,
		widget = wibox.container.background,
	})

	-- Stack: main unten, overlay oben
	local container = wibox.widget({
		root_widget,
		overlay_root,
		layout = wibox.layout.stack,
	})

	-- Popup-Shell (ohne Chrome/Layout; bg transparent)
	local popup = awful.popup({
		widget = container,
		visible = false,
		ontop = true,
		bg = "#00000000",
		type = "dock",
	})

	----------------------------------------------------------------
	-- ESC-Keygrabber (nur Cycle)
	----------------------------------------------------------------
	local esc_id = nil
	local function start_esc()
		if esc_id then
			return
		end
		esc_id = awful.keygrabber.run(function(_, key, ev)
			if ev ~= "press" then
				return
			end
			if key == "Escape" then
				-- Kontextsensitiv: zuerst Dialog zu, sonst Menü zu
				if overlay_root.visible then
					overlay_root.visible = false
					overlay_slot:reset()
					awesome.emit_signal("menu::dialog_closed")
				else
					popup.visible = false
				end
			end
		end)
	end
	local function stop_esc()
		if esc_id then
			pcall(awful.keygrabber.stop, esc_id)
			esc_id = nil
		end
	end

	----------------------------------------------------------------
	-- Outside-Click (Cycle)
	----------------------------------------------------------------
	local saved_root_buttons, outside_root_buttons
	local client_click_connected = false

	local function is_inside_popup(px, py)
		local gx, gy, gw, gh = popup.x, popup.y, popup.width, popup.height
		return px >= gx and px <= gx + gw and py >= gy and py <= gy + gh
	end

	local function try_close_on_xy(x, y)
		if popup.visible and not is_inside_popup(x, y) then
			popup.visible = false
			return true
		end
		return false
	end

	local function install_outside_listeners()
		if not saved_root_buttons then
			saved_root_buttons = root.buttons()
			local function on_root_click()
				local c = mouse.coords()
				try_close_on_xy(c.x, c.y)
			end
			outside_root_buttons = gears.table.join(
				awful.button({}, 1, on_root_click),
				awful.button({}, 2, on_root_click),
				awful.button({}, 3, on_root_click)
			)
			root.buttons(gears.table.join(saved_root_buttons or {}, outside_root_buttons))
		end
		if not client_click_connected then
			client.connect_signal("button::press", function()
				local pos = mouse.coords()
				try_close_on_xy(pos.x, pos.y)
			end)
			client_click_connected = true
		end
	end

	local function remove_outside_listeners()
		if saved_root_buttons then
			root.buttons(saved_root_buttons)
			saved_root_buttons = nil
			outside_root_buttons = nil
		end
		client_click_connected = false
	end

	----------------------------------------------------------------
	-- Öffentliche API (nur Cycle + Overlay)
	----------------------------------------------------------------
	local api = {}
	local placement_fn = nil -- vom Aufrufer gesetzt (layout gehört NICHT hierher)

	function api:set_placement(fn)
		placement_fn = fn
	end

	function api:is_visible()
		return popup.visible
	end

	function api:show(opts2)
		local s = (opts2 and opts2.screen) or mouse.screen or awful.screen.focused()
		popup.screen = s
		popup.visible = true
		install_outside_listeners()
		start_esc()
		-- Platzierung delegiert an init.lua (delayed tick)
		if placement_fn then
			gears.timer.delayed_call(function()
				placement_fn(popup, s, opts2)
			end)
		end
	end

	function api:hide()
		if not popup.visible then
			return
		end
		popup.visible = false
		remove_outside_listeners()
		stop_esc()
		overlay_root.visible = false
		overlay_slot:reset()
		if type(on_hide) == "function" then
			pcall(on_hide, api)
		end
	end

	function api:toggle(opts2)
		if popup.visible then
			self:hide()
		else
			self:show(opts2)
		end
	end

	-- Dialog-Overlay (layoutneutral; Widget muss bereits korrekt gebaut sein)
	function api:show_dialog(widget)
		if not widget then
			return
		end
		if not is_widget(widget) then
			local ok, built = pcall(wibox.widget, widget)
			if not ok or not is_widget(built) then
				return
			end
			widget = built
		end
		overlay_slot:reset()
		overlay_slot:add(widget)
		overlay_root.visible = true
	end

	function api:hide_dialog()
		overlay_root.visible = false
		overlay_slot:reset()
	end

	-- Scrim: nur Dialog schließen
	overlay_root:buttons(gears.table.join(awful.button({}, 1, function()
		api:hide_dialog()
		awesome.emit_signal("menu::dialog_closed")
	end)))

	popup:connect_signal("property::visible", function()
		if not popup.visible then
			stop_esc()
			remove_outside_listeners()
			overlay_root.visible = false
			overlay_slot:reset()
			if type(on_hide) == "function" then
				pcall(on_hide, api)
			end
		end
	end)

	-- Nur Re-Placement hooken – die Logik liefert init.lua
	screen.connect_signal("property::workarea", function(s)
		if popup.visible and popup.screen == s and placement_fn then
			placement_fn(popup, s, nil)
		end
	end)

	return api
end

-- Back-Compat: build(args) -> wrap(align-vertical(header,cols,footer))
function Popup.build(args)
	local header, cols, footer = args.header, args.cols, args.footer
	local root = wibox.widget({ header, cols, footer, layout = wibox.layout.align.vertical })
	return Popup.wrap(root, { on_hide = args.on_hide, scrim_bg = args.scrim_bg })
end

function Popup.make_launcher(api, icon, beautiful)
	return wibox.widget({
		image = icon or (beautiful and beautiful.awesome_icon),
		widget = wibox.widget.imagebox,
		buttons = gears.table.join(awful.button({}, 1, function()
			api:toggle()
		end)),
	})
end

return Popup
