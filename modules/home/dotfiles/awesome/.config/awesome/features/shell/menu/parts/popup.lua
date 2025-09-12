-- ~/.config/awesome/features/shell/menu/parts/popup.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}

-- args = { header, cols, footer, theme, placement }  -- placement: fn(p, s, opts)
function Popup.build(args)
	local header = args.header
	local cols = args.cols
	local footer = args.footer
	local t = args.theme or {}

	-- Fallback-Placement: links, über der unteren Bar (workarea-aware)
	local place_fn = args.placement
		or function(p, s)
			local wa = s.workarea or s.geometry
			local gap = 2
			local ph = (p.height and p.height > 0) and p.height or (t.total_height or 520)
			p.x = wa.x
			p.y = wa.y + wa.height - gap - ph
		end

	-- Container (nicht "root" nennen!)
	local container = wibox.widget({
		header,
		cols,
		footer,
		layout = wibox.layout.align.vertical,
	})

	local popup = awful.popup({
		widget = container,
		visible = false,
		ontop = true,
		shape = t.shape or gears.shape.rectangle,
		border_width = t.border_width or 1,
		border_color = t.border_color or "#000000",
		bg = t.bg or "#222222",
		fg = t.fg or "#ffffff",
		minimum_width = (t.col_left_w or 320) + (t.col_right_w or 220) + (t.cols_pad_l or 6) + (t.cols_pad_r or 6),
		minimum_height = t.total_height or 520,
	})

	-- --------- Outside-Click via temporäre Root-Buttons (kein Grab, kein Backdrop)
	local saved_root_buttons = nil
	local outside_bindings = nil

	local function install_outside_bindings(api, popup_obj)
		if saved_root_buttons then
			return
		end
		saved_root_buttons = root.buttons()

		local function maybe_close()
			if not popup_obj.visible then
				return
			end
			local c = mouse.coords()
			local x, y = c.x, c.y
			local gx, gy = popup_obj.x, popup_obj.y
			local gw, gh = popup_obj.width, popup_obj.height
			local inside = x >= gx and x <= gx + gw and y >= gy and y <= gy + gh
			if not inside then
				api:hide()
			end
		end

		outside_bindings = gears.table.join(
			awful.button({}, 1, maybe_close), -- Left click
			awful.button({}, 2, maybe_close), -- Middle
			awful.button({}, 3, maybe_close) -- Right
		)

		-- WICHTIG: Wir ersetzen nicht, wir hängen an
		root.buttons(gears.table.join(saved_root_buttons or {}, outside_bindings))
	end

	local function remove_outside_bindings()
		if not saved_root_buttons then
			return
		end
		-- Ursprüngliche Root-Buttons wiederherstellen
		root.buttons(saved_root_buttons)
		saved_root_buttons = nil
		outside_bindings = nil
	end
	-- -----------------------------------------------------------------------

	-- Helfer: sichere Platzierung mit Retry/Clamping
	local function place_safe(popup_obj, s, place, opts, theme)
		local wa = s.workarea or s.geometry
		local gap = 2
		local ph = (popup_obj.height and popup_obj.height > 0) and popup_obj.height or (theme.total_height or 650)
		ph = math.min(ph, math.max(wa.height - gap * 2, 1)) -- nie höher als Workarea

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

	-- Öffentliche API
	local api = {}

	-- opts: { screen=<screen> }
	function api:show(opts)
		local s = (opts and opts.screen) or mouse.screen or awful.screen.focused()
		popup.screen = s
		popup.visible = true

		-- Platzierung, bis Maße/Workarea stabil
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

		-- Outside-Click aktivieren (blockiert nichts)
		install_outside_bindings(api, popup)
	end

	function api:hide()
		if not popup.visible then
			return
		end
		popup.visible = false
		remove_outside_bindings()
	end

	function api:toggle(opts)
		if popup.visible then
			self:hide()
		else
			self:show(opts)
		end
	end

	-- Safety: falls Popup anderswo unsichtbar wird
	popup:connect_signal("property::visible", function()
		if not popup.visible then
			remove_outside_bindings()
		end
	end)

	-- Bei Workarea-Änderung neu platzieren, falls sichtbar
	screen.connect_signal("property::workarea", function(s)
		if popup.visible and popup.screen == s then
			place_safe(popup, s, place_fn, nil, t)
		end
	end)

	return api
end

return Popup
