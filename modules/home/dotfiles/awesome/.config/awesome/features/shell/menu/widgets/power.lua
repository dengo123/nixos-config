local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local theme = require("features.shell.menu.widgets.theme")
local helper = require("features.shell.menu.widgets.helpers")

local M = {}

function M.power_button(btn, t)
	t = theme.with_defaults(t)
	local eff_h = t._power_inner_h or t.power_h
	local pad_t = t.power_pad_t or 0
	local pad_b = t.power_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = theme.resolve_icon_size(t, avail_h, "power")
	local font = theme.resolve_font(t, avail_h, "power")

	local inner = wibox.widget({
		{
			image = btn.icon,
			resize = true,
			forced_height = icon_px,
			forced_width = icon_px,
			widget = wibox.widget.imagebox,
		},
		{ text = btn.text or "", font = font, valign = "center", widget = wibox.widget.textbox },
		spacing = t.power_spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	local placed = wibox.widget({ inner, halign = "left", valign = "center", widget = wibox.container.place })

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

	helper.apply_hover(box, t, t.power_bg, t.power_bg_hover)

	if not (t.defer_power_clicks or btn.no_default_click) then
		box:buttons(gears.table.join(awful.button({}, 1, function()
			if btn.on_press then
				btn.on_press()
			end
		end)))
	end

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

function M.power_bar(power_items, t, opts)
	t = theme.with_defaults(t)
	opts = opts or {}
	local inner_h = opts.inner_h or t.footer_h or 48
	local bar = { layout = wibox.layout.fixed.horizontal, spacing = t.power_bar_spacing }

	for _, p in ipairs(power_items or {}) do
		local t_btn = {}
		for k, v in pairs(t) do
			t_btn[k] = v
		end
		t_btn.defer_power_clicks = true
		t_btn._power_inner_h = inner_h

		local btn = M.power_button(p, t_btn)
		local fixed = wibox.widget({ btn, strategy = "exact", height = inner_h, widget = wibox.container.constraint })

		local inner = btn._click_target or btn
		inner:buttons({})
		btn:buttons({})

		local raw = (p.text or p.label or ""):lower()
		local key = (p.id or raw):gsub("%s+", ""):lower()

		local function bind(fn)
			local b = gears.table.join(awful.button({}, 1, fn))
			inner:buttons(b)
			btn:buttons(b)
		end

		local matched = false
		if opts.dialogs then
			if key == "power" or raw:find("shutdown") or raw:find("turnoff") then
				matched = true
				bind(function()
					opts.dialogs.power({
						bg = t.footer_bg or t.bg,
						fg = t.footer_fg or t.fg,
						btn_bg = t.dialog_btn_bg or "#ECECEC",
						btn_fg = t.dialog_btn_fg or "#000000",
						backdrop = t.dialog_backdrop or "#00000088",
						radius = t.dialog_radius or 6,
					})
				end)
			elseif key == "logout" or key == "logoff" or raw:find("logout") or raw:find("exit") then
				matched = true
				bind(function()
					opts.dialogs.logout_confirm({
						bg = t.footer_bg or t.bg,
						fg = t.footer_fg or t.fg,
						btn_bg = t.dialog_btn_bg or "#ECECEC",
						btn_fg = t.dialog_btn_fg or "#000000",
						backdrop = t.dialog_backdrop or "#00000088",
						radius = t.dialog_radius or 6,
					})
				end)
			end
		end
		if not matched and p.on_press then
			bind(p.on_press)
		end

		table.insert(bar, fixed)
	end

	return wibox.widget({ bar, halign = "right", widget = wibox.container.place })
end

return M
