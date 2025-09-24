-- ~/.config/awesome/features/shell/menu/widgets/rows.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local theme = require("features.shell.menu.widgets.theme")
local Lib = require("features.shell.menu.lib") -- nur der Aggregator

local M = {}

-- weicher Fallback für fixed_height, falls lib.helpers.fixed_height fehlt
local function fallback_fixed_height(widget, h)
	return wibox.widget({
		widget,
		strategy = "exact",
		height = math.max(1, tonumber(h) or 1),
		widget = wibox.container.constraint,
	})
end

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

function M.row_widget(item, t, opts)
	t = theme.with_defaults(t)
	opts = opts or {}

	local lib = resolve_lib(opts)
	local helpers = (lib and lib.helpers) or {}
	local actions = (lib and lib.actions) or nil

	local eff_h = t.row_h
	local pad_t = t.row_pad_t or 0
	local pad_b = t.row_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = theme.resolve_icon_size(t, avail_h, "rows")
	local font = theme.resolve_font(t, avail_h, "rows")

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

	-- inner Background (für Hover)
	local bg_box = wibox.widget({
		content,
		bg = t.row_bg,
		fg = t.row_fg,
		widget = wibox.container.background,
	})

	-- äußerer Focus-Rahmen (für Tastatur-Fokus)
	local focus_wrap = wibox.widget({
		bg = "#00000000",
		shape = t.row_shape or t.shape or gears.shape.rectangle,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})
	focus_wrap:set_widget(bg_box)

	-- Hover nur anwenden, wenn vorhanden
	if type(helpers.apply_hover) == "function" then
		-- NEU: wenn t.unify_focus_hover == true, HOVER NICHT separat anwenden
		if not t.unify_focus_hover then
			helpers.apply_hover(bg_box, t, t.row_bg, t.row_bg_hover)
		end
	end

	-- Klick strikt über Lib.actions (falls vorhanden), sonst No-Op
	local on_click = nil
	if actions and type(actions.click) == "function" then
		on_click = actions.click(item)
	end
	local bindings = gears.table.join(awful.button({}, 1, on_click or function() end))
	bg_box:buttons(bindings)
	-- optional vollflächig:
	focus_wrap:buttons(bindings)

	-- Optionale Hover-Callbacks aus Item beibehalten
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

	-- focus API auf dem Focus-Container implementieren
	function focus_wrap:set_focus(on, th2)
		local tt = th2 or t

		-- Farben & Form
		local bg_on = tt.row_focus_bg or tt.row_bg_hover or tt.bg_focus or "#FFFFFF22"
		local fg_on = tt.row_focus_fg or tt.row_fg
		local bg_off = tt.row_bg
		local fg_off = tt.row_fg

		-- Rundung der "Button"-Fläche (innerer Container)
		local shape_on = tt.row_focus_shape or tt.row_shape or tt.shape or gears.shape.rounded_rect
		local radius_on = tonumber(tt.row_focus_radius or tt.row_radius) or 8

		-- optional: extra Innenabstand im Fokus (wirkt „button-y“)
		local fpl = tonumber(tt.row_focus_pad_l or tt.row_pad_l) or 0
		local fpr = tonumber(tt.row_focus_pad_r or tt.row_pad_r) or 0
		local fpt = tonumber(tt.row_focus_pad_t or tt.row_pad_t) or 0
		local fpb = tonumber(tt.row_focus_pad_b or tt.row_pad_b) or 0

		-- Button-Highlight: auf dem INNEREN bg_box arbeiten,
		-- und den äußeren Focus-Rahmen komplett neutral lassen.
		if on then
			bg_box.bg = bg_on
			bg_box.fg = fg_on
			bg_box.shape = function(cr, w, h)
				shape_on(cr, w, h, radius_on)
			end
			bg_box.shape_clip = true
			-- optional: falls du KEINEN Border willst, sorge dafür, dass der äußere Wrap nichts zeichnet:
			focus_wrap.bg = "#00000000"
			focus_wrap.shape_border_width = 0

			-- leicht mehr Innenabstand im Fokus (fühlt sich wie ein Button an)
			if content and content.set_left then
				content.left = fpl
				content.right = fpr
				content.top = fpt
				content.bottom = fpb
			end
		else
			bg_box.bg = bg_off
			bg_box.fg = fg_off
			-- zurück zur neutralen Form (kann rechteckig bleiben, oder selbe Rundung)
			bg_box.shape = function(cr, w, h)
				local r = tonumber(tt.row_radius) or 0
				local base_shape = tt.row_shape or tt.shape or gears.shape.rectangle
				base_shape(cr, w, h, r)
			end
			bg_box.shape_clip = false

			focus_wrap.bg = "#00000000"
			focus_wrap.shape_border_width = 0

			-- Standard-Padding zurücksetzen
			if content and content.set_left then
				content.left = tt.row_pad_l or content.left
				content.right = tt.row_pad_r or content.right
				content.top = tt.row_pad_t or content.top
				content.bottom = tt.row_pad_b or content.bottom
			end
		end
	end

	function focus_wrap:activate()
		if on_click then
			on_click()
		end
	end

	-- fixed_height über Lib.helpers, sonst Fallback
	local out
	if type(helpers.fixed_height) == "function" then
		out = helpers.fixed_height(focus_wrap, eff_h)
	else
		out = fallback_fixed_height(focus_wrap, eff_h)
	end

	-- >>> WICHTIG: Fokus/Activate nach außen durchreichen,
	-- weil 'out' (constraint) das eigentliche Widget ist, das Focus.attach bekommt
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
	t = theme.with_defaults(t)

	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing }
	local focus = {}

	for _, it in ipairs(items or {}) do
		local row = M.row_widget(it, t, opts)
		table.insert(box, row)
		table.insert(focus, row) -- Leaf liefert :set_focus/:activate (per Proxy)
	end

	return wibox.widget(box), focus
end

return M
