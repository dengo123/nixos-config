-- ~/.config/awesome/features/shell/menu/widgets/rows.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- Fallback, falls kein helpers.fixed_height vorhanden ist
local function fallback_fixed_height(widget, h)
	return wibox.widget({
		widget,
		strategy = "exact",
		height = math.max(1, tonumber(h) or 1),
		widget = wibox.container.constraint,
	})
end

-- Dependencies (styler + lib) aus opts ziehen, mit schlanken Fallbacks
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
	local lib = deps.lib -- darf nil sein
	return styler, lib
end

-- item: { icon, text, on_hover_in?, on_hover_out?, on_press? ... }
-- t:    theme table (via styler.with_defaults normalisiert)
-- opts: { colors = {bg, fg, hover}, row_h = number, deps = { styler, lib } }
function M.row_widget(item, t, opts)
	local styler, lib = resolve_deps(opts)
	t = styler.with_defaults(t or {})
	opts = opts or {}

	-- unified focus/hover: Tastaturfokus == Maushover (default: an)
	local unify = (t.unify_focus_hover ~= false)

	local helpers = (lib and lib.helpers) or {}

	-- Farben/Höhe (i. d. R. von Columns gesetzt)
	local base_bg = t.row_bg or "#00000000"
	local base_fg = t.row_fg or "#FFFFFF"
	local COLORS = opts.colors
		or {
			bg = base_bg,
			fg = base_fg,
			hover = t.row_bg_hover or styler.adjust(base_bg, -8),
		}
	-- sichere Defaults
	COLORS.bg = COLORS.bg or base_bg
	COLORS.fg = COLORS.fg or base_fg
	COLORS.hover = COLORS.hover or COLORS.bg

	local eff_h = tonumber(opts.row_h) or tonumber(t.row_h) or 48

	-- Innenabstände & Metriken
	local pad_t = t.row_pad_t or 0
	local pad_b = t.row_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)
	local icon_px = styler.resolve_icon_size(t, avail_h, "rows")
	local font = styler.resolve_font(t, avail_h, "rows")

	-- Inhalt
	local hline = wibox.widget({
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
		spacing = t.row_spacing or 8,
		layout = wibox.layout.fixed.horizontal,
	})

	local placed = wibox.widget({
		hline,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local content = wibox.widget({
		placed,
		left = t.row_pad_l or 0,
		right = t.row_pad_r or 0,
		top = pad_t,
		bottom = pad_b,
		widget = wibox.container.margin,
	})

	-- sichtbare Fläche der Row
	local bg_box = wibox.widget({
		content,
		bg = COLORS.bg,
		fg = COLORS.fg,
		widget = wibox.container.background,
	})

	-- Fokus-Hülle (deckt die ganze Row)
	local focus_wrap = wibox.widget({
		bg = "#00000000",
		shape = t.row_shape or t.shape or gears.shape.rectangle,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})
	focus_wrap:set_widget(bg_box)

	-- Hover-Handling
	local is_kb_focus = false
	if not unify and type(helpers.apply_hover) == "function" then
		-- alter Weg (nur wenn explizit gewünscht)
		helpers.apply_hover(bg_box, t, COLORS.bg, COLORS.hover)
	else
		-- unified: auf der ganzen Row-Hülle lauschen
		focus_wrap:connect_signal("mouse::enter", function()
			if not is_kb_focus then
				bg_box.bg = COLORS.hover
				-- bg_box.fg = t.row_focus_fg or COLORS.fg -- optional
			end
		end)
		focus_wrap:connect_signal("mouse::leave", function()
			if not is_kb_focus then
				bg_box.bg = COLORS.bg
				bg_box.fg = COLORS.fg
			end
		end)
	end

	-- Klick-Callback: lib.actions.click(item) ODER item.on_press
	local actions = (lib and lib.actions) or nil
	local on_click = (actions and type(actions.click) == "function" and actions.click(item)) or item.on_press or nil

	local bindings = gears.table.join(awful.button({}, 1, on_click or function() end))
	bg_box:buttons(bindings)
	focus_wrap:buttons(bindings)

	-- optionale Item-Hooks
	bg_box:connect_signal("mouse::enter", function()
		if item.on_hover_in then
			item.on_hover_in(item, bg_box)
		end
	end)
	bg_box:connect_signal("mouse::leave", function()
		if item.on_hover_out then
			item.on_hover_out(item, bg_box)
		end
	end)

	-- Für Mouse→KB-Fokus-Glue (wichtig!)
	focus_wrap.mouse_enter_target = focus_wrap

	-- Tastatur-Fokus == Hover-Look
	function focus_wrap:set_focus(on)
		is_kb_focus = not not on
		if on then
			bg_box.bg = COLORS.hover
			bg_box.fg = t.row_focus_fg or COLORS.fg
		else
			bg_box.bg = COLORS.bg
			bg_box.fg = COLORS.fg
		end
		-- keine Rahmen
		focus_wrap.bg = "#00000000"
		focus_wrap.shape_border_width = 0
		bg_box.shape = nil
		bg_box.shape_clip = false
	end

	function focus_wrap:activate()
		if on_click then
			on_click()
		end
	end

	-- Höhe fixieren
	local out = (helpers and type(helpers.fixed_height) == "function") and helpers.fixed_height(focus_wrap, eff_h)
		or fallback_fixed_height(focus_wrap, eff_h)

	-- Proxy-Methoden (für Focus.attach)
	function out:set_focus(on, th2)
		if focus_wrap.set_focus then
			focus_wrap:set_focus(on, th2)
		end
	end

	function out:activate()
		if focus_wrap.activate then
			focus_wrap:activate()
		end
	end

	return out
end

function M.list_widget(items, t, opts)
	local styler = resolve_deps(opts) -- nur den ersten Rückgabewert (styler) verwenden
	t = styler.with_defaults(t or {})
	opts = opts or {}

	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing or 4 }
	local focus = {}

	for _, it in ipairs(items or {}) do
		-- pro Row exakt die von Columns gesetzten opts.colors/row_h weitergeben
		local row = M.row_widget(it, t, {
			colors = (it.colors or opts.colors),
			row_h = (it.row_h or opts.row_h),
			deps = opts.deps, -- wichtig: deps weiterreichen
		})
		table.insert(box, row)
		table.insert(focus, row)
	end

	return wibox.widget(box), focus
end

return M
