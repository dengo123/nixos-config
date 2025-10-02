-- ~/.config/awesome/shell/launchers/lib/cancel.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- ======== EINZIGE Quelle für Design/Größe ========
local STYLE = {
	height = 24, -- feste Höhe in px
	min_width = 96, -- Mindestbreite in px
	radius = 2, -- Eckenradius
	pad_h = 14, -- interner horizontaler Padding
	font = "Sans 11", -- Schrift
	bg = "#F5F5EE", -- Grundhintergrund
	fg = "#111111", -- Textfarbe
	hover_bg = "#EDEDE6", -- Hover-Hintergrund
	border = "#2B77FF", -- Hover-/Focus-Randfarbe
	border_bw = 3, -- Hover-/Focus-Randbreite
	label = "Cancel", -- Default-Text
}

local function pick(a, b)
	return a ~= nil and a or b
end

-- Baut einen Cancel-Button. Einziger Look-Override ist der Text (label).
function M.mk_cancel_button(label, on_click)
	local lbl = wibox.widget({
		text = pick(label, STYLE.label),
		font = STYLE.font,
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- Innenleben mit horizontalem Padding
	local padded = wibox.widget({
		{
			lbl,
			left = STYLE.pad_h,
			right = STYLE.pad_h,
			widget = wibox.container.margin,
		},
		bg = STYLE.bg,
		fg = STYLE.fg,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, STYLE.radius)
		end,
		shape_border_width = 0,
		shape_border_color = "#00000000",
		widget = wibox.container.background,
	})

	-- Fixe Höhe + Mindestbreite erzwingen
	local fixed_h = wibox.widget({
		padded,
		strategy = "exact",
		height = STYLE.height,
		widget = wibox.container.constraint,
	})

	local root = wibox.widget({
		fixed_h,
		strategy = "min",
		width = STYLE.min_width,
		widget = wibox.container.constraint,
	})

	-- Hover
	local function to_hover()
		padded.bg = STYLE.hover_bg
		padded.shape_border_width = STYLE.border_bw
		padded.shape_border_color = STYLE.border
	end
	local function to_idle()
		padded.bg = STYLE.bg
		padded.shape_border_width = 0
		padded.shape_border_color = "#00000000"
	end

	root:connect_signal("mouse::enter", to_hover)
	root:connect_signal("mouse::leave", to_idle)

	-- Click
	root:buttons(gears.table.join(awful.button({}, 1, function()
		if type(on_click) == "function" then
			on_click()
		end
	end)))

	-- Tastatur-Fokus-API (für evtl. spätere Navigation)
	function root:set_focus(on)
		if on then
			to_hover()
		else
			to_idle()
		end
	end

	function root:activate()
		if type(on_click) == "function" then
			on_click()
		end
	end

	root.mouse_enter_target = root

	return root
end

return M
