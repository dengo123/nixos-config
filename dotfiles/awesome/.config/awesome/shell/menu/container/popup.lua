-- ~/.config/awesome/shell/menu/container/popup.lua
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

-- opts: {
--   theme?, scrim_bg?, placement?, on_hide?, enable_esc_grabber?,
--   window_shape_radius?   -- Radius fürs Fenster (fallback: theme.popup_radius oder 12)
-- }
function Popup.wrap(root_widget, opts)
	assert(is_widget(root_widget), "Popup.wrap: root_widget must be a widget")
	opts = opts or {}
	local t = opts.theme or {}
	local on_hide = opts.on_hide
	local enable_esc = (opts.enable_esc_grabber ~= false)
	local scrim_bg = opts.scrim_bg or t.dialog_scrim or "#00000099"
	local R = tonumber(opts.window_shape_radius or t.popup_radius) or 12

	-- Overlay
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

	-- Root + Overlay
	local container = wibox.widget({
		root_widget,
		overlay_root,
		layout = wibox.layout.stack,
	})

	-- Popup-Shell (transparent) + echte Fensterform setzen
	local popup = awful.popup({
		widget = container,
		visible = false,
		ontop = true,
		bg = "#00000000",
		type = "dock",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, R)
		end,
	})

	-- Eingabe-Maske ebenfalls runden (Ecken sind nicht klickbar)
	if popup.set_input_shape then
		popup:set_input_shape(function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, R)
		end)
	elseif popup.shape_input ~= nil then
		popup.shape_input = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, R)
		end
	end

	-- Default-Platzierung
	local place_fn = opts.placement
		or function(p, s)
			local wa = s.workarea or s.geometry
			local gap = 2
			local ph = (p.height and p.height > 0) and p.height or (t.total_height or 520)
			p.x = wa.x
			p.y = wa.y + wa.height - gap - ph
		end

	-- Outside-Click schließt
	local saved_root_buttons = nil
	local client_click_connected = false
	local on_client_click = nil

	local function install_outside(api)
		if not saved_root_buttons then
			saved_root_buttons = root.buttons()
			local function on_root_click()
				if popup.visible then
					api:hide()
				end
			end
			root.buttons(
				gears.table.join(
					saved_root_buttons or {},
					awful.button({}, 1, on_root_click),
					awful.button({}, 2, on_root_click),
					awful.button({}, 3, on_root_click)
				)
			)
		end

		if not client_click_connected then
			on_client_click = function()
				if popup.visible then
					api:hide()
				end
			end
			client.connect_signal("button::press", on_client_click)
			client_click_connected = true
		end
	end

	local function remove_outside()
		if saved_root_buttons then
			root.buttons(saved_root_buttons)
			saved_root_buttons = nil
		end
		if client_click_connected and on_client_click then
			client.disconnect_signal("button::press", on_client_click)
			on_client_click = nil
			client_click_connected = false
		end
	end

	-- ESC
	local esc_id = nil
	local function start_esc()
		if not enable_esc or esc_id then
			return
		end
		esc_id = awful.keygrabber.run(function(_, key, ev)
			if ev ~= "press" then
				return
			end
			if key == "Escape" then
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

	-- Re-Placement Helper (ohne minimum_height setzen)
	local function do_place(p, s, o, pf)
		if p and p.valid and pf then
			pf(p, s, o)
		end
	end

	-- Öffentliche API
	local api = {}
	local placement_fn = nil

	function api:set_placement(fn)
		placement_fn = fn
	end

	function api:is_visible()
		return popup.visible
	end

	function api:show(o)
		local s = (o and o.screen) or mouse.screen or awful.screen.focused()
		popup.screen = s
		popup.visible = true
		install_outside(api)
		start_esc()

		local pf = placement_fn or place_fn

		-- 1) sofort versuchen
		do_place(popup, s, o, pf)

		-- 2) im nächsten Tick (nach erstem Layout)
		gears.timer.delayed_call(function()
			do_place(popup, s, o, pf)
		end)

		-- 3) einmalig nach dem ersten echten Layout-Change der Root-Hierarchie
		local once_id
		once_id = container:connect_signal("widget::layout_changed", function()
			if once_id then
				container:disconnect_signal("widget::layout_changed", once_id)
				once_id = nil
				do_place(popup, s, o, pf)
			end
		end)
	end

	function api:hide()
		if not popup.visible then
			return
		end
		popup.visible = false
		remove_outside()
		stop_esc()
		overlay_root.visible = false
		overlay_slot:reset()
		if type(on_hide) == "function" then
			pcall(on_hide, api)
		end
	end

	function api:toggle(o)
		if popup.visible then
			self:hide()
		else
			self:show(o)
		end
	end

	function api:show_dialog(w)
		if not w then
			return
		end
		if not is_widget(w) then
			local ok, b = pcall(wibox.widget, w)
			if not ok then
				return
			end
			w = b
		end
		overlay_slot:reset()
		overlay_slot:add(w)
		overlay_root.visible = true
	end

	function api:hide_dialog()
		if overlay_root.visible then
			overlay_root.visible = false
			overlay_slot:reset()
		end
	end

	overlay_root:buttons(gears.table.join(awful.button({}, 1, function()
		api:hide_dialog()
		awesome.emit_signal("menu::dialog_closed")
	end)))

	-- Bei Geometrieänderung die Form erneut setzen (defensiv) und ggf. neu platzieren
	popup:connect_signal("property::geometry", function(p)
		if p and p.valid then
			p.shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, R)
			end
			if p.set_input_shape then
				p:set_input_shape(function(cr, w, h)
					gears.shape.rounded_rect(cr, w, h, R)
				end)
			elseif p.shape_input ~= nil then
				p.shape_input = function(cr, w, h)
					gears.shape.rounded_rect(cr, w, h, R)
				end
			end
			-- Re-Placement nach echter Größenänderung
			local pf = placement_fn or place_fn
			local s = p.screen or awful.screen.focused()
			do_place(p, s, nil, pf)
		end
	end)

	screen.connect_signal("property::workarea", function(s)
		if popup.visible and popup.screen == s then
			local pf = placement_fn or place_fn
			if pf then
				pf(popup, s, nil)
			end
		end
	end)

	return api
end

-- Back-Compat für alten Build-Pfad
function Popup.build(args)
	local root = wibox.widget({
		args.header,
		args.cols,
		args.footer,
		layout = wibox.layout.align.vertical,
	})
	return Popup.wrap(root, {
		theme = args.theme,
		placement = args.placement,
		on_hide = args.on_hide,
		enable_esc_grabber = (args.enable_esc_grabber ~= false),
		scrim_bg = args.scrim_bg,
		window_shape_radius = args.window_shape_radius,
	})
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
