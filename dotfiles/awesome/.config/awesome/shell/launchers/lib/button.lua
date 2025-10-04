-- ~/.config/awesome/shell/launchers/lib/button.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local B = {}

local STYLE = {
	-- feste Größe
	height = 24,
	width = 96, -- << NEU: feste Breite
	radius = 4, -- Außenradius
	pad_h = 14,
	font = "Sans 11",
	fg = "#111111",

	-- Basisfläche
	base_bg = "#FFFFFF",

	-- permanenter Außenrand
	outer_bw = 1,
	outer_col = "#000000",

	-- Hover-Ring (innen)
	hover_col = "#2B77FF",
	hover_bw = 1,
	hover_inset = 2,
}

local function hex_rgb(hex)
	hex = (hex or "#000000"):gsub("#", "")
	local r = tonumber(hex:sub(1, 2), 16) / 255
	local g = tonumber(hex:sub(3, 4), 16) / 255
	local b = tonumber(hex:sub(5, 6), 16) / 255
	return r, g, b
end

function B.mk_button(label, on_click, style_override)
	local S = setmetatable(style_override or {}, { __index = STYLE })

	-- Text
	local txt = wibox.widget({
		text = label or "Button",
		font = S.font,
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- Innenradius = Außenradius - Außenrand
	local inner_r = math.max(0, (S.radius or 0) - (S.outer_bw or 0))

	-- weiße Basisfläche mit zentriertem Text
	local base = wibox.widget({
		{
			{ txt, widget = wibox.container.place },
			left = S.pad_h,
			right = S.pad_h,
			widget = wibox.container.margin,
		},
		bg = S.base_bg,
		fg = S.fg,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, inner_r)
		end,
		shape_clip = true,
		widget = wibox.container.background,
	})

	-- Hover-Overlay als Canvas, zeichnet nur wenn _hover_on=true
	local overlay = wibox.widget.base.make_widget()
	overlay._hover_on = false
	overlay.fit = function(_, _, w, h)
		return w, h
	end
	overlay.draw = function(self, _, cr, w, h)
		if not self._hover_on then
			return
		end
		local inset = (S.outer_bw or 0) + (S.hover_inset or 0)
		local ww, hh = w - inset * 2, h - inset * 2
		if ww <= 0 or hh <= 0 then
			return
		end
		cr:save()
		cr:translate(inset, inset)
		local r = math.max(0, inner_r - (S.hover_inset or 0))
		gears.shape.rounded_rect(cr, ww, hh, r)
		local rr, gg, bb = hex_rgb(S.hover_col)
		cr:set_source_rgb(rr, gg, bb)
		cr:set_line_width(S.hover_bw or 1)
		cr:stroke()
		cr:restore()
	end

	-- Stack: Basis unten, Hover oben
	local inner_stack = wibox.widget({
		base,
		overlay,
		layout = wibox.layout.stack,
	})

	-- permanenter Außenrand
	local outer = wibox.widget({
		{
			inner_stack,
			left = (S.outer_bw or 0),
			right = (S.outer_bw or 0),
			top = (S.outer_bw or 0),
			bottom = (S.outer_bw or 0),
			widget = wibox.container.margin,
		},
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, S.radius or 0)
		end,
		shape_clip = true,
		shape_border_width = S.outer_bw or 1,
		shape_border_color = S.outer_col or "#000000",
		bg = "#00000000",
		widget = wibox.container.background,
	})

	-- feste Höhe + feste Breite (beides exact)
	local fixed_h = wibox.widget({ outer, strategy = "exact", height = S.height, widget = wibox.container.constraint })
	local fixed = wibox.widget({ fixed_h, strategy = "exact", width = S.width, widget = wibox.container.constraint })

	local root = fixed

	-- Hover-Flag togglen + redraw (Events sicher am Root registrieren)
	local function set_hover(on)
		overlay._hover_on = not not on
		overlay:emit_signal("widget::redraw_needed")
	end
	root:connect_signal("mouse::enter", function()
		set_hover(true)
	end)
	root:connect_signal("mouse::leave", function()
		set_hover(false)
	end)

	-- Klick
	root:buttons(gears.table.join(awful.button({}, 1, function()
		if type(on_click) == "function" then
			on_click()
		end
	end)))

	function root:set_focus(on)
		set_hover(on)
	end

	function root:activate()
		if type(on_click) == "function" then
			on_click()
		end
	end

	set_hover(false) -- initial
	return root
end

return B
