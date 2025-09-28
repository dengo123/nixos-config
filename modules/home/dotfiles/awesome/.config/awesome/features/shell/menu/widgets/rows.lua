-- ~/.config/awesome/features/shell/menu/widgets/rows.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local Theme = require("features.shell.menu.lib.theme")
local Lib = require("features.shell.menu.lib")

local M = {}

-- Fallback, falls lib.helpers.fixed_height fehlt
local function fallback_fixed_height(widget, h)
	return wibox.widget({
		widget,
		strategy = "exact",
		height = math.max(1, tonumber(h) or 1),
		widget = wibox.container.constraint,
	})
end

-- Lib-Auflösung
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

-- item: { icon, text, ... }
-- t:    theme table (wird mit Defaults normalisiert)
-- opts: { colors = {bg, fg, hover}, row_h = number, lib = ... }
function M.row_widget(item, t, opts)
	if not (t and t.__raw_theme) then
		t = Theme.with_defaults(t)
	end
	opts = opts or {}
	-- unified focus/hover: Tastatur-Fokus soll exakt wie Maus-Hover aussehen (default: an)
	local unify = (t.unify_focus_hover ~= false)

	local lib = resolve_lib(opts)
	local helpers = (lib and lib.helpers) or {}

	-- *** WICHTIG: Farben/Höhe kommen fertig aus Columns (opts.colors/row_h) ***
	local COLORS = opts.colors
		or {
			bg = t.row_bg,
			fg = t.row_fg,
			hover = t.row_bg_hover or Theme.adjust(t.row_bg, -8),
		}
	local eff_h = tonumber(opts.row_h) or tonumber(t.row_h) or 48

	-- Innenabstände & Metriken
	local pad_t = t.row_pad_t or 0
	local pad_b = t.row_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)
	local icon_px = Theme.resolve_icon_size(t, avail_h, "rows")
	local font = Theme.resolve_font(t, avail_h, "rows")

	-- Inhalt
	local hline = wibox.widget({
		{
			image = item.icon,
			resize = true,
			forced_height = icon_px,
			forced_width = icon_px,
			widget = wibox.widget.imagebox,
		},
		{ text = item.text or "", font = font, valign = "center", widget = wibox.widget.textbox },
		spacing = t.row_spacing,
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
		left = t.row_pad_l,
		right = t.row_pad_r,
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

	-- Fokus-Hülle (ohne Rahmen)
	local focus_wrap = wibox.widget({
		bg = "#00000000",
		shape = t.row_shape or t.shape or gears.shape.rectangle,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})
	focus_wrap:set_widget(bg_box)

	-- --- Hover-Handling ---
	-- unified: wir zeichnen Hover in set_focus() und steuern Maus separat so,
	--          dass bei Maus-Over derselbe Look entsteht, aber Keyboard-Fokus Vorrang hat.
	local is_kb_focus = false
	if not unify and type(helpers.apply_hover) == "function" then
		-- alter Weg (nur wenn explizit gewünscht)
		helpers.apply_hover(bg_box, t, COLORS.bg, COLORS.hover)
	else
		-- unified hover
		bg_box:connect_signal("mouse::enter", function()
			if not is_kb_focus then
				bg_box.bg = COLORS.hover or COLORS.bg
				-- fg optional ändern, falls gewünscht:
				-- bg_box.fg = t.row_focus_fg or COLORS.fg
			end
		end)
		bg_box:connect_signal("mouse::leave", function()
			if not is_kb_focus then
				bg_box.bg = COLORS.bg
				bg_box.fg = COLORS.fg
			end
		end)
	end

	-- Klick via Lib.actions (falls vorhanden)
	local actions = (lib and lib.actions) or nil
	local on_click = (actions and type(actions.click) == "function") and actions.click(item) or nil
	local bindings = gears.table.join(awful.button({}, 1, on_click or function() end))
	bg_box:buttons(bindings)
	focus_wrap:buttons(bindings)

	-- optionale Item-Hooks beibehalten
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

	-- Tastatur-Fokus == Hover-Look
	function focus_wrap:set_focus(on)
		is_kb_focus = not not on
		if on then
			bg_box.bg = COLORS.hover or COLORS.bg
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
	local out = (type(helpers.fixed_height) == "function") and helpers.fixed_height(focus_wrap, eff_h)
		or fallback_fixed_height(focus_wrap, eff_h)

	-- Proxy Methoden (wichtig für Focus.attach)
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
	if not (t and t.__raw_theme) then
		t = Theme.with_defaults(t)
	end
	opts = opts or {}

	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing }
	local focus = {}

	for _, it in ipairs(items or {}) do
		-- pro Row exakt die von Columns gesetzten opts.colors/row_h weitergeben
		local row = M.row_widget(it, t, {
			colors = (it.colors or opts.colors),
			row_h = (it.row_h or opts.row_h),
			lib = opts.lib,
		})
		table.insert(box, row)
		table.insert(focus, row)
	end

	return wibox.widget(box), focus
end

return M
