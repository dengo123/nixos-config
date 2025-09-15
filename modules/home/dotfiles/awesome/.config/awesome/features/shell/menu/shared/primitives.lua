-- ~/.config/awesome/features/shell/menu/shared/primitives.lua
-- (falls deine Datei nicht unter shared/ liegt, passe den Pfad an oder verschiebe sie.)
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local P = {}

-- ---------- Farb-Utils ----------
local function clamp(x, a, b)
	return math.max(a, math.min(b, x))
end
local function hex_to_rgb(hex)
	hex = hex or "#000000"
	local r, g, b = hex:match("#?(%x%x)(%x%x)(%x%x)")
	return tonumber(r, 16), tonumber(g, 16), tonumber(b, 16)
end
local function rgb_to_hex(r, g, b)
	return string.format(
		"#%02X%02X%02X",
		clamp(math.floor(r + 0.5), 0, 255),
		clamp(math.floor(g + 0.5), 0, 255),
		clamp(math.floor(b + 0.5), 0, 255)
	)
end
local function adjust(hex, pct)
	local r, g, b = hex_to_rgb(hex)
	local f = 1 + (pct / 100)
	return rgb_to_hex(r * f, g * f, b * f)
end

-- ---------- Defaults ----------
local DEFAULTS = {
	bg = "#3A6EA5",
	fg = "#FFFFFF",
	bg_focus = nil,

	-- Rows (Columns)
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	row_bg_hover = nil,
	row_h = 48,
	list_spacing = 0, -- kein sichtbarer Abstand zwischen Reihen

	-- Footer power buttons
	footer_bg = "#3A6EA5",
	footer_fg = "#FFFFFF",
	power_bg = nil,
	power_fg = nil,
	power_bg_hover = nil,
	power_w = 110,
	power_h = 48,
	power_icon_size = 16,
}

local function with_defaults(t)
	t = t or {}
	for k, v in pairs(DEFAULTS) do
		if t[k] == nil then
			t[k] = v
		end
	end
	t.power_bg = t.power_bg or t.footer_bg or t.bg
	t.power_fg = t.power_fg or t.footer_fg or t.fg
	t.bg_focus = t.bg_focus or adjust(t.bg, -15)
	t.row_bg_hover = t.row_bg_hover or adjust(t.row_bg, -8)
	t.power_bg_hover = t.power_bg_hover or adjust(t.power_bg, -12)
	return t
end

-- ---------- Hover Helper ----------
function P.apply_hover(bg_container, t, normal, hover)
	t = with_defaults(t)
	local normal_bg = normal or t.bg
	local hover_bg = hover or t.bg_focus
	if normal_bg:lower() == hover_bg:lower() then
		hover_bg = adjust(normal_bg, -12)
	end
	bg_container:connect_signal("mouse::enter", function()
		bg_container.bg = hover_bg
	end)
	bg_container:connect_signal("mouse::leave", function()
		bg_container.bg = normal_bg
	end)
end

-- ---------- Fixhöhe-Wrapper ----------
function P.fixed_height(widget, h)
	return wibox.widget({
		widget,
		strategy = "exact",
		height = h,
		widget = wibox.container.constraint,
	})
end

-- ---------- LISTEN-ROW / COLUMN-BUTTON ----------
function P.row_widget(item, t)
	t = with_defaults(t)
	local icon_size = t.icon_size or 18

	local content = wibox.widget({
		{
			{
				image = item.icon,
				resize = true,
				forced_height = icon_size,
				forced_width = icon_size,
				widget = wibox.widget.imagebox,
			},
			{
				text = item.text or "",
				widget = wibox.widget.textbox,
			},
			spacing = t.row_spacing or 8,
			layout = wibox.layout.fixed.horizontal,
		},
		left = t.row_pad_l or 10,
		right = t.row_pad_r or 10,
		top = t.row_pad_t or 4,
		bottom = t.row_pad_b or 4,
		widget = wibox.container.margin,
	})

	local bg_box = wibox.widget({
		content,
		bg = t.row_bg,
		fg = t.row_fg,
		widget = wibox.container.background,
	})

	P.apply_hover(bg_box, t, t.row_bg, t.row_bg_hover)

	bg_box:buttons(gears.table.join(awful.button({}, 1, function()
		if item.on_press then
			item.on_press()
		end
	end)))

	return P.fixed_height(bg_box, t.row_h)
end

function P.list_widget(items, t)
	t = with_defaults(t)
	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing }
	for _, it in ipairs(items or {}) do
		table.insert(box, P.row_widget(it, t))
	end
	return wibox.widget(box)
end

-- ---------- FOOTER-POWER-BUTTON ----------
function P.power_button(btn, t)
	t = with_defaults(t)
	local size = t.power_icon_size

	local inner = wibox.widget({
		{
			image = btn.icon,
			resize = true,
			forced_height = size,
			forced_width = size,
			widget = wibox.widget.imagebox,
		},
		{
			text = btn.text or "",
			widget = wibox.widget.textbox,
		},
		spacing = t.power_spacing or 6,
		layout = wibox.layout.fixed.horizontal,
	})

	local box = wibox.widget({
		{
			inner,
			left = t.power_pad_l or 10,
			right = t.power_pad_r or 10,
			top = t.power_pad_t or 4,
			bottom = t.power_pad_b or 4,
			widget = wibox.container.margin,
		},
		bg = t.power_bg,
		fg = t.power_fg,
		shape = t.power_shape or t.shape or gears.shape.rectangle,
		widget = wibox.container.background,
	})

	-- Hover
	P.apply_hover(box, t, t.power_bg, t.power_bg_hover)

	-- *** WICHTIG: Default-Click NUR wenn NICHT defered ***
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

	-- Dem Aufrufer (Footer) das echte Click-Target geben:
	fixed._click_target = box

	return fixed
end

return P
