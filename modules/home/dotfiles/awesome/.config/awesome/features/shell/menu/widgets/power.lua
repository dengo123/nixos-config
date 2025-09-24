-- ~/.config/awesome/features/shell/menu/widgets/power.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local theme = require("features.shell.menu.lib.theme")
local Lib = require("features.shell.menu.lib") -- nur der Aggregator

local M = {}

-- Lib-Auflösung: opts.lib > opts.api.lib > __menu_api.lib > require'd Lib
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

-- Reiner UI-Button (keine Logik)
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

	-- Hover nur anwenden, wenn vorhanden
	if type(helpers.apply_hover) == "function" then
		helpers.apply_hover(box, t, t.power_bg, t.power_bg_hover)
	end

	-- Keine Default-Logik hier – Clicks werden außen gebunden!
	local fixed = wibox.widget({
		box,
		strategy = "exact",
		width = t.power_w,
		height = t.power_h,
		widget = wibox.container.constraint,
	})

	fixed._click_target = box
	return fixed
end

-- Rechte Power-Leiste (UI-only) – Clicks -> Lib.actions.click(item)
function M.power_bar(power_items, t, opts)
	t = theme.with_defaults(t)
	opts = opts or {}

	local lib = resolve_lib(opts)
	local actions = (lib and lib.actions) or nil

	local inner_h = opts.inner_h or t.footer_h or 48
	local bar = { layout = wibox.layout.fixed.horizontal, spacing = t.power_bar_spacing }

	for _, item in ipairs(power_items or {}) do
		-- Button-Theme für diese Höhe
		local t_btn = {}
		for k, v in pairs(t) do
			t_btn[k] = v
		end
		t_btn._power_inner_h = inner_h

		-- Button bauen
		local btn = M.power_button(item, t_btn, opts)
		local fixed = wibox.widget({
			btn,
			strategy = "exact",
			height = inner_h,
			widget = wibox.container.constraint,
		})

		-- existierende Bindings entfernen (Safety)
		local target = btn._click_target or btn
		target:buttons({})
		btn:buttons({})

		-- EIN Handler: zentral über Lib.actions (falls vorhanden), sonst No-Op
		local cb = (actions and type(actions.click) == "function") and actions.click(item) or function() end
		local bindings = gears.table.join(awful.button({}, 1, cb))
		target:buttons(bindings)
		btn:buttons(bindings)

		table.insert(bar, fixed)
	end

	return wibox.widget({ bar, halign = "right", widget = wibox.container.place })
end

return M
