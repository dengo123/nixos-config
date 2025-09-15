-- ~/.config/awesome/features/shell/menu/shared/dialogs.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Dialogs = {}

-- Button Helper -------------------------------------------------------------
local function mk_button(label, opts, on_press)
	opts = opts or {}
	local txt = wibox.widget({
		text = label,
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})
	local bg = wibox.widget({
		{
			txt,
			left = opts.pad_l or 14,
			right = opts.pad_r or 14,
			top = opts.pad_t or 8,
			bottom = opts.pad_b or 8,
			widget = wibox.container.margin,
		},
		bg = opts.btn_bg or "#ECECEC",
		fg = opts.btn_fg or "#000000",
		shape = gears.shape.rounded_rect,
		widget = wibox.container.background,
	})
	local normal = opts.btn_bg or "#ECECEC"
	local hover = opts.btn_bg_hover or "#DADADA"
	bg:connect_signal("mouse::enter", function()
		bg.bg = hover
	end)
	bg:connect_signal("mouse::leave", function()
		bg.bg = normal
	end)
	bg:buttons(gears.table.join(awful.button({}, 1, function()
		if on_press then
			on_press()
		end
	end)))
	return bg
end

-- Popup + Backdrop (Awesome 4.3 kompatibel) --------------------------------
local function show_dialog(content_widget, opts)
	opts = opts or {}
	local s = awful.screen.focused()

	-- Backdrop dimmt den Root
	local backdrop = wibox({
		screen = s,
		visible = true,
		ontop = true,
		type = "splash",
		bg = opts.backdrop or "#00000088",
	})
	backdrop:geometry(s.geometry)

	-- Dialog-Popup: widget direkt im Konstruktor übergeben
	local popup = awful.popup({
		screen = s,
		ontop = true,
		visible = false,
		type = "dialog",
		bg = opts.bg or "#2E3440",
		fg = opts.fg or "#ECEFF4",
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, opts.radius or 8)
		end,
		placement = awful.placement.centered,
		minimum_width = opts.width or 420,
		maximum_width = opts.max_w or 640,

		widget = {
			{
				content_widget,
				left = opts.pad_l or 16,
				right = opts.pad_r or 16,
				top = opts.pad_t or 16,
				bottom = opts.pad_b or 16,
				widget = wibox.container.margin,
			},
			widget = wibox.container.background,
		},
	})

	popup.visible = true
	awful.placement.centered(popup, { honor_workarea = true })

	local function close()
		if popup and popup.visible then
			popup.visible = false
		end
		if backdrop and backdrop.visible then
			backdrop.visible = false
		end
	end

	backdrop:buttons(gears.table.join(awful.button({}, 1, function()
		close()
	end)))
	awful
		.keygrabber({
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
		:start()

	return { close = close, popup = popup, backdrop = backdrop }
end

-- POWER (Shut down, Restart, Hibernate, Cancel) -----------------------------
function Dialogs.power(opts)
	opts = opts or {}
	local title = wibox.widget({ markup = "<b>Power</b>", align = "center", widget = wibox.widget.textbox })
	local msg = wibox.widget({ text = "Was möchtest du tun?", align = "center", widget = wibox.widget.textbox })

	local row = wibox.widget({ spacing = opts.btn_spacing or 12, layout = wibox.layout.fixed.horizontal })

	local dlg
	local function finish()
		if dlg then
			dlg.close()
		end
	end

	local btn_shutdown = mk_button("Shut down", opts, function()
		finish()
		awful.spawn.with_shell("systemctl poweroff")
	end)
	local btn_reboot = mk_button("Restart", opts, function()
		finish()
		awful.spawn.with_shell("systemctl reboot")
	end)
	local btn_suspend = mk_button("Hibernate", opts, function()
		finish()
		awful.spawn.with_shell("systemctl hibernate")
	end)
	local btn_cancel = mk_button("Cancel", opts, function()
		finish()
	end)

	row:add(btn_shutdown)
	row:add(btn_reboot)
	row:add(btn_suspend)
	row:add(btn_cancel)

	local content = wibox.widget({
		title,
		wibox.widget({ forced_height = 6, widget = wibox.container.constraint }),
		msg,
		wibox.widget({ forced_height = 12, widget = wibox.container.constraint }),
		row,
		layout = wibox.layout.fixed.vertical,
	})

	dlg = show_dialog(content, opts)
	return dlg
end

-- LOGOUT (soft: nur Awesome beenden) ---------------------------------------
function Dialogs.logout_confirm(opts)
	opts = opts or {}
	local title = wibox.widget({ markup = "<b>Logout</b>", align = "center", widget = wibox.widget.textbox })
	local msg = wibox.widget({ text = "Fenstermanager beenden?", align = "center", widget = wibox.widget.textbox })

	local row = wibox.widget({ spacing = opts.btn_spacing or 12, layout = wibox.layout.fixed.horizontal })

	local dlg
	local function finish()
		if dlg then
			dlg.close()
		end
	end

	local btn_exit = mk_button("Exit WM", opts, function()
		finish()
		awesome.quit()
	end)
	local btn_cancel = mk_button("Cancel", opts, function()
		finish()
	end)

	row:add(btn_exit)
	row:add(btn_cancel)

	local content = wibox.widget({
		title,
		wibox.widget({ forced_height = 6, widget = wibox.container.constraint }),
		msg,
		wibox.widget({ forced_height = 12, widget = wibox.container.constraint }),
		row,
		layout = wibox.layout.fixed.vertical,
	})

	dlg = show_dialog(content, opts)
	return dlg
end

return Dialogs
