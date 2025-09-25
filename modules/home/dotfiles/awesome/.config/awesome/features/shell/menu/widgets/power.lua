-- ~/.config/awesome/features/shell/menu/widgets/power.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local theme = require("features.shell.menu.lib.theme")
local Lib = require("features.shell.menu.lib")

local M = {}

local function resolve_lib(opts)
	opts = opts or {}
	if opts.lib then
		return opts.lib
	end
	if opts.api and opts.api.lib then
		return opts.api.lib
	end
	local api = rawget(_G, "__menu_api")
	if api and api.lib then
		return api.lib
	end
	return Lib
end

-- Ein einzelner Button, UI + Fokus/hover Logik, aber noch ohne Aktion
function M.power_button(item, t, opts)
	t = theme.with_defaults(t)
	opts = opts or {}

	local lib = resolve_lib(opts)
	local helpers = (lib and lib.helpers) or {}

	local eff_h = t._power_inner_h or t.power_h
	local pad_t = t.power_pad_t or 0
	local pad_b = t.power_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = theme.resolve_icon_size(t, avail_h, "power")
	local font = theme.resolve_font(t, avail_h, "power")

	local inner = wibox.widget({
		{
			image = item.icon,
			resize = true,
			forced_height = icon_px,
			forced_width = icon_px,
			widget = wibox.widget.imagebox,
		},
		{
			text = item.text or "",
			font = font,
			valign = "center",
			widget = wibox.widget.textbox,
		},
		spacing = t.power_spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	local placed = wibox.widget({
		inner,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local box = wibox.widget({
		{
			placed,
			left = t.power_pad_l,
			right = t.power_pad_r,
			top = pad_t,
			bottom = pad_b,
			widget = wibox.container.margin,
		},
		bg = t.power_bg,
		fg = t.power_fg,
		shape = t.power_shape or t.shape or gears.shape.rectangle,
		widget = wibox.container.background,
	})

	local fixed = wibox.widget({
		box,
		strategy = "exact",
		width = t.power_w,
		height = t.power_h,
		widget = wibox.container.constraint,
	})

	-- >>> unified Fokus/hover
	function fixed:set_focus(on, th2)
		local tt = theme.with_defaults(th2 or t)
		local bg_on = tt.power_bg_hover or theme.adjust(tt.power_bg, -12)
		if on then
			box.bg, box.fg = bg_on, tt.power_fg
		else
			box.bg, box.fg = tt.power_bg, tt.power_fg
		end
	end

	fixed:set_focus(false, t)

	if t.unify_focus_hover then
		box:connect_signal("mouse::enter", function()
			fixed:set_focus(true, t)
		end)
		box:connect_signal("mouse::leave", function()
			fixed:set_focus(false, t)
		end)
	else
		if type(helpers.apply_hover) == "function" then
			helpers.apply_hover(box, t, t.power_bg, t.power_bg_hover)
		end
	end

	-- Für Focus.attach: Maus-Follow soll auf dem sichtbaren Kasten reagieren
	fixed.mouse_enter_target = box

	-- Wird in power_bar mit echter Aktion überschrieben
	function fixed:activate() end

	fixed._click_target = box
	return fixed
end

-- Power-Bar: gibt jetzt (widget, focus_list) zurück!
function M.power_bar(power_items, t, opts)
	t = theme.with_defaults(t)
	opts = opts or {}

	local lib = resolve_lib(opts)
	local actions = (lib and lib.actions) or nil

	local inner_h = opts.inner_h or t.footer_h or 48
	local row = { layout = wibox.layout.fixed.horizontal, spacing = t.power_bar_spacing }
	local focus_list = {}

	for _, item in ipairs(power_items or {}) do
		local t_btn = {}
		for k, v in pairs(t) do
			t_btn[k] = v
		end
		t_btn._power_inner_h = inner_h

		local btn = M.power_button(item, t_btn, opts)

		-- Höhe auf die Footer-Row zwingen
		local fixed_h = wibox.widget({
			btn,
			strategy = "exact",
			height = inner_h,
			widget = wibox.container.constraint,
		})

		-- (Sicherheits-)Bindings leeren
		local target = btn._click_target or btn
		target:buttons({})
		btn:buttons({})

		-- Genau eine Aktion: zentral via Lib.actions
		local cb = (actions and type(actions.click) == "function") and actions.click(item) or function() end
		local bindings = gears.table.join(awful.button({}, 1, cb))
		target:buttons(bindings)
		btn:buttons(bindings)

		-- Tastatur-Activate soll dasselbe tun wie Klick
		function btn:activate()
			cb()
		end

		table.insert(row, fixed_h)
		table.insert(focus_list, btn) -- <<< WICHTIG: Fokus-Item ist der Button mit set_focus/activate
	end

	local placed = wibox.widget({ row, halign = "right", widget = wibox.container.place })
	return placed, focus_list -- <<< neu: Fokusliste mitgeben
end

return M
