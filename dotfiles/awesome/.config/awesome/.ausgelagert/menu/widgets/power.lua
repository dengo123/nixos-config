-- ~/.config/awesome/shell/menu/widgets/power.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- Kleiner Helfer: Dependencies (Styler + Lib) aus opts herausziehen.
-- Bietet schlanke Fallbacks, falls nichts injiziert wurde.
local function resolve_deps(opts)
	opts = opts or {}
	local deps = opts.deps or (opts.api and opts.api.deps) or {}

	local styler = deps.styler
		or {
			with_defaults = function(t)
				return t or {}
			end,
			adjust = function(hex, _)
				return hex
			end,
			resolve_icon_size = function(_, h)
				return math.max(1, math.floor((h or 24) * 0.6 + 0.5))
			end,
			resolve_font = function(_, h)
				return string.format("Sans %d", math.max(8, math.floor((h or 24) * 0.35 + 0.5)))
			end,
		}

	local lib = deps.lib -- darf nil sein; dann gibt es einfach keine Actions

	return styler, lib
end

-- Ein einzelner Button, UI + unified Fokus/Hover, noch ohne Aktion
function M.power_button(item, t, opts)
	local styler = resolve_deps(opts)
	t = styler.with_defaults(t)
	opts = opts or {}

	local eff_h = t._power_inner_h or t.power_h or 48
	local pad_t = t.power_pad_t or 0
	local pad_b = t.power_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = styler.resolve_icon_size(t, avail_h, "power")
	local font = styler.resolve_font(t, avail_h, "power")

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
		spacing = t.power_spacing or 8,
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
			left = t.power_pad_l or 0,
			right = t.power_pad_r or 0,
			top = pad_t,
			bottom = pad_b,
			widget = wibox.container.margin,
		},
		bg = t.power_bg or "#00000000",
		fg = t.power_fg or "#FFFFFF",
		shape = t.power_shape or t.shape or gears.shape.rectangle,
		widget = wibox.container.background,
	})

	local fixed = wibox.widget({
		box,
		strategy = "exact",
		width = t.power_w or nil,
		height = t.power_h or nil,
		widget = wibox.container.constraint,
	})

	-- Unified Fokus/Hover (Keyboard == Mouse)
	function fixed:set_focus(on, th2)
		local tt = styler.with_defaults(th2 or t)
		local bg_on = tt.power_bg_hover or styler.adjust(tt.power_bg or "#00000000", -12)
		if on then
			box.bg, box.fg = bg_on, tt.power_fg or "#FFFFFF"
		else
			box.bg, box.fg = tt.power_bg or "#00000000", tt.power_fg or "#FFFFFF"
		end
	end

	fixed:set_focus(false, t)

	-- Maus-Events mappen 1:1 auf set_focus
	box:connect_signal("mouse::enter", function()
		fixed:set_focus(true, t)
	end)
	box:connect_signal("mouse::leave", function()
		fixed:set_focus(false, t)
	end)

	-- Für Focus.attach: Fokus soll beim sichtbaren Kasten „einrasten“
	fixed.mouse_enter_target = box

	-- Wird in power_bar mit echter Aktion überschrieben
	function fixed:activate() end

	-- Für Click-Bindings
	fixed._click_target = box
	return fixed
end

-- Power-Bar: gibt (widget, focus_list) zurück
function M.power_bar(power_items, t, opts)
	local styler, lib = resolve_deps(opts)
	t = styler.with_defaults(t)
	opts = opts or {}

	local inner_h = opts.inner_h or t.footer_h or 48
	local row = { layout = wibox.layout.fixed.horizontal, spacing = t.power_bar_spacing or 8 }
	local focus_list = {}

	for _, item in ipairs(power_items or {}) do
		-- Button-Theme an die Footer-Höhe anpassen
		local t_btn = {}
		for k, v in pairs(t) do
			t_btn[k] = v
		end
		t_btn._power_inner_h = inner_h

		-- Deps an Kinder weiterreichen
		local child_opts = { deps = opts.deps }

		local btn = M.power_button(item, t_btn, child_opts)

		-- Höhe auf Footer-Row zwingen
		local fixed_h = wibox.widget({
			btn,
			strategy = "exact",
			height = inner_h,
			widget = wibox.container.constraint,
		})

		-- Sicherheits-Bindings leeren
		local target = btn._click_target or btn
		target:buttons({})
		btn:buttons({})

		-- Genau eine Aktion: zentral via Lib.actions (falls vorhanden), sonst No-Op
		local actions = lib and lib.actions or nil
		local cb = (actions and type(actions.click) == "function") and actions.click(item) or function() end
		local bindings = gears.table.join(awful.button({}, 1, cb))
		target:buttons(bindings)
		btn:buttons(bindings)

		-- Tastatur-Activate == Klick
		function btn:activate()
			cb()
		end

		table.insert(row, fixed_h)
		table.insert(focus_list, btn) -- Fokus-Item ist der Button selbst
	end

	local placed = wibox.widget({ row, halign = "right", widget = wibox.container.place })
	return placed, focus_list
end

return M
