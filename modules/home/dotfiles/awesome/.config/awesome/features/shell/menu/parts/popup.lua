-- ~/.config/awesome/features/shell/menu/parts/popup.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}

-- helper oben einfügen (falls nicht vorhanden)
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- args = { header, cols, footer, theme, placement, on_hide }
function Popup.build(args)
	local header = args.header
	local cols = args.cols
	local footer = args.footer
	local t = args.theme or {}
	local on_hide = args.on_hide

	-- Fallback-Placement
	local place_fn = args.placement
		or function(p, s)
			local wa = s.workarea or s.geometry
			local gap = 2
			local ph = (p.height and p.height > 0) and p.height or (t.total_height or 520)
			p.x = wa.x
			p.y = wa.y + wa.height - gap - ph
		end

	-- Container
	local container = wibox.widget({
		header,
		cols,
		footer,
		layout = wibox.layout.align.vertical,
	})

	-- Popup selbst mit Rundung
	local RADIUS = t.popup_radius or 12
	local popup = awful.popup({
		widget = container,
		visible = false,
		ontop = true,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, RADIUS)
		end,

		-- >>> hier: Theme-konform statt Schwarz/Default
		border_width = pick(t.popup_border_width, 1),
		border_color = pick(t.popup_border_color, t.header_bg, "#3A6EA5"),
		bg = pick(t.popup_bg, "#00000000"), -- transparent, damit columns.border_bg sichtbar ist
		fg = pick(t.fg, "#ffffff"),

		minimum_width = (t.col_left_w or 200) + (t.col_right_w or 200) + (t.cols_pad_l or 6) + (t.cols_pad_r or 6),
		minimum_height = t.total_height or 650,
	})

	-- Outside-Klick Handling ------------------------------------------------
	local saved_root_buttons, outside_root_buttons
	local client_click_connected = false

	local function is_inside_popup(px, py)
		local gx, gy = popup.x, popup.y
		local gw, gh = popup.width, popup.height
		return px >= gx and px <= gx + gw and py >= gy and py <= gy + gh
	end

	local function try_close_on_xy(x, y, api)
		if popup.visible and not is_inside_popup(x, y) then
			api:hide()
			return true
		end
		return false
	end

	local function install_outside_listeners(api)
		if not saved_root_buttons then
			saved_root_buttons = root.buttons()
			local function on_root_click()
				local c = mouse.coords()
				try_close_on_xy(c.x, c.y, api)
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
				try_close_on_xy(pos.x, pos.y, api)
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
		if client_click_connected then
			client_click_connected = false
		end
	end
	-------------------------------------------------------------------------

	local function place_safe(popup_obj, s, place, opts, theme)
		local wa = s.workarea or s.geometry
		local gap = 2
		local ph = (popup_obj.height and popup_obj.height > 0) and popup_obj.height or (theme.total_height or 650)
		ph = math.min(ph, math.max(wa.height - gap * 2, 1))
		if popup_obj.height ~= ph then
			popup_obj:geometry({ height = ph })
		end
		place(popup_obj, s, opts)
		if popup_obj.y == nil then
			popup_obj.y = wa.y + wa.height - gap - ph
		end
		if popup_obj.x == nil then
			popup_obj.x = wa.x
		end
	end

	-- *** NEU: ESC-Keygrabber für Menü-Popup ***
	local esc_grabber = nil
	local function start_esc_grabber(api)
		if esc_grabber then
			return
		end
		esc_grabber = awful.keygrabber.run(function(mod, key, event)
			if event == "release" then
				return
			end
			if key == "Escape" then
				api:hide()
			end
		end)
	end
	local function stop_esc_grabber()
		if esc_grabber then
			awful.keygrabber.stop(esc_grabber)
			esc_grabber = nil
		end
	end

	-- API -------------------------------------------------------------------
	local api = {}

	function api:show(opts)
		local s = (opts and opts.screen) or mouse.screen or awful.screen.focused()
		popup.screen = s
		popup.visible = true

		local attempts, max_attempts = 0, 6
		local function try_place()
			if not popup.visible then
				return false
			end
			attempts = attempts + 1
			place_safe(popup, s, place_fn, opts, t)
			if popup.height > 0 or attempts >= max_attempts then
				return false
			end
			return true
		end
		gears.timer.delayed_call(try_place)
		gears.timer.start_new(0.016, try_place)

		install_outside_listeners(api)
		start_esc_grabber(api) -- <<< NEU
	end

	function api:hide()
		if not popup.visible then
			return
		end
		if on_hide then
			pcall(on_hide)
		end
		popup.visible = false
		remove_outside_listeners()
		stop_esc_grabber() -- <<< NEU
	end

	function api:toggle(opts)
		if popup.visible then
			self:hide()
		else
			self:show(opts)
		end
	end

	popup:connect_signal("property::visible", function()
		if not popup.visible then
			if on_hide then
				pcall(on_hide)
			end
			remove_outside_listeners()
			stop_esc_grabber() -- <<< failsafe
		end
	end)

	screen.connect_signal("property::workarea", function(s)
		if popup.visible and popup.screen == s then
			place_safe(popup, s, place_fn, nil, t)
		end
	end)

	return api
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
