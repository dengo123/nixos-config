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

	-- Backdrop: fängt Outside-Klicks ab (liegt nur über der Workarea)
	local backdrop = wibox({
		visible = false,
		ontop = true, -- über Clients
		type = "utility", -- unauffälliger Fenstertyp
		bg = "#00000000", -- volle Transparenz
		opacity = 0.0, -- sicherstellen, dass nix verdeckt wird
		shape = gears.shape.rectangle,
	})

	-- Popup selbst: liegt über dem Backdrop
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

	-- Outside-Klick -> schließen
	backdrop:buttons(gears.table.join(
		awful.button({}, 1, function()
			popup.visible = false
			backdrop.visible = false
		end),
		awful.button({}, 3, function()
			popup.visible = false
			backdrop.visible = false
		end)
	))

	-- Helfer: sichere Platzierung mit Retry/Clamping
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

	-- Backdrop an Screen-Workarea anpassen
	local function size_backdrop(s)
		local wa = s.workarea or s.geometry
		backdrop:geometry({ x = wa.x, y = wa.y, width = wa.width, height = wa.height })
		backdrop.screen = s
	end

	-- Öffentliche API
	local api = {}

	-- opts: { screen=<screen> }
	function api:show(opts)
		local s = (opts and opts.screen) or mouse.screen or awful.screen.focused()

		size_backdrop(s)
		backdrop.visible = true -- erst Backdrop …

		popup.screen = s
		popup.visible = true -- … dann Popup drüber

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
	end

	function api:hide()
		if not popup.visible and not backdrop.visible then
			return
		end
		popup.visible = false
		backdrop.visible = false
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
			backdrop.visible = false
		end
	end)

	-- Screen-Größe ändert sich -> Backdrop + ggf. Popup neu platzieren
	screen.connect_signal("property::workarea", function(s)
		if backdrop.visible and backdrop.screen == s then
			size_backdrop(s)
		end
		if popup.visible and popup.screen == s then
			place_safe(popup, s, place_fn, nil, t)
		end
	end)

	return api
end

return Popup
