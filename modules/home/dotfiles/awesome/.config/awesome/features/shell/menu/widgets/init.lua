-- ~/.config/awesome/features/shell/menu/parts/widgets.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local P = {}

-- ========== Utils: Farben ==========
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

-- ========== Defaults (Standardisierte Keys) ==========
-- Schema: {scope}_{prop}, z.B. header_h, row_pad_l, power_icon_size
local DEFAULTS = {
	-- Grundfarben
	bg = "#235CDB",
	fg = "#FFFFFF",
	bg_focus = nil,

	-- Globale Typografie/Icon Ratios (overrides: *_header, *_rows, *_power)
	font_family = "Sans",
	text_ratio = 0.25, -- Standard-Text-Ratio (0..1)
	text_ratio_header = nil,
	text_ratio_rows = nil,
	text_ratio_power = nil,
	text_size = nil, -- absolute Größe global
	text_size_header = nil,
	text_size_rows = nil,
	text_size_power = nil,
	text_min = nil, -- clamp optional
	text_max = nil,

	icon_ratio = 0.80, -- Standard-Icon-Ratio (0..1)
	icon_ratio_header = nil,
	icon_ratio_rows = nil,
	icon_ratio_power = nil,
	-- Absolute Größen bleiben kompatibel, werden aber von Ratios übersteuert
	avatar_size = nil, -- alias für header icon size
	icon_size = nil, -- global absolute icon size (Rows)
	icon_size_header = nil,
	icon_size_rows = nil,
	icon_size_power = nil,

	-- Header Layout
	header_h = 64,
	header_bg = nil, -- fallback: t.bg
	header_fg = nil, -- fallback: t.fg
	header_pad_l = 10,
	header_pad_r = 10,
	header_pad_t = 8,
	header_pad_b = 8,
	header_spacing = 10,
	header_text_spacing = 2,
	avatar_radius = 8,

	-- Rows (Columns)
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	row_bg_hover = nil,
	row_h = 48,
	row_pad_l = 10,
	row_pad_r = 10,
	row_pad_t = 4,
	row_pad_b = 4,
	row_spacing = 8,
	list_spacing = 0,

	-- Footer / Power
	footer_bg = "#235CDB",
	footer_fg = "#FFFFFF",
	power_bg = nil, -- fallback: footer_bg
	power_fg = nil, -- fallback: footer_fg
	power_bg_hover = nil,
	power_w = 110,
	power_h = 48,
	power_pad_l = 10,
	power_pad_r = 10,
	power_pad_t = 4,
	power_pad_b = 4,
	power_spacing = 6,
	power_icon_size = nil, -- alt; bleibt kompatibel (weniger bevorzugt wenn Ratio gesetzt)
	power_bar_spacing = 0, -- spacing zwischen Buttons in der Leiste
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

-- ========== Größen-Resolver (Ratio > Absolute > Fallback) ==========
local function resolve_text_size_number(t, eff_h, kind)
	-- 1) absolute
	if kind == "header" and t.text_size_header then
		return t.text_size_header
	end
	if kind == "rows" and t.text_size_rows then
		return t.text_size_rows
	end
	if kind == "power" and t.text_size_power then
		return t.text_size_power
	end
	if t.text_size then
		return t.text_size
	end
	-- 2) ratio
	local ratio = t.text_ratio
	if kind == "header" and t.text_ratio_header then
		ratio = t.text_ratio_header
	end
	if kind == "rows" and t.text_ratio_rows then
		ratio = t.text_ratio_rows
	end
	if kind == "power" and t.text_ratio_power then
		ratio = t.text_ratio_power
	end
	local sz = math.max(1, math.floor(eff_h * (ratio or DEFAULTS.text_ratio) + 0.5))
	if t.text_min then
		sz = math.max(sz, t.text_min)
	end
	if t.text_max then
		sz = math.min(sz, t.text_max)
	end
	return sz
end

local function resolve_font(t, eff_h, kind)
	local family = t.font_family or "Sans"
	local size = resolve_text_size_number(t, eff_h, kind)
	return family .. " " .. tostring(size)
end

local function resolve_icon_size(t, eff_h, kind)
	-- 1) kind-spezifische absolute
	if kind == "header" then
		if t.icon_size_header then
			return t.icon_size_header
		end
		if t.avatar_size then
			return t.avatar_size
		end -- Back-compat alias
	elseif kind == "rows" then
		if t.icon_size_rows then
			return t.icon_size_rows
		end
	elseif kind == "power" then
		if t.icon_size_power then
			return t.icon_size_power
		end
		if t.power_icon_size then
			return t.power_icon_size
		end -- Back-compat alias
	end
	-- 2) globale absolute
	if t.icon_size then
		return t.icon_size
	end
	-- 3) ratio (kind-spezifisch > global)
	local ratio = t.icon_ratio
	if kind == "header" and t.icon_ratio_header then
		ratio = t.icon_ratio_header
	end
	if kind == "rows" and t.icon_ratio_rows then
		ratio = t.icon_ratio_rows
	end
	if kind == "power" and t.icon_ratio_power then
		ratio = t.icon_ratio_power
	end
	ratio = ratio or DEFAULTS.icon_ratio
	return math.max(1, math.floor(eff_h * ratio + 0.5))
end

-- (optional) export der Resolver für spätere Nutzung
P.resolve_font = resolve_font
P.resolve_icon_size = resolve_icon_size
P.resolve_text_size_number = resolve_text_size_number

-- ========== Hover & Helpers ==========
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

function P.fixed_height(widget, h)
	return wibox.widget({ widget, strategy = "exact", height = h, widget = wibox.container.constraint })
end

-- ========== ROWS (Columns) ==========
-- item = { icon=..., text=..., on_press=function() ... end }
function P.row_widget(item, t)
	t = with_defaults(t)
	local eff_h = t.row_h or 48
	local pad_t = t.row_pad_t or 0
	local pad_b = t.row_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = resolve_icon_size(t, avail_h, "rows")
	local font = resolve_font(t, avail_h, "rows")

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

	local bg_box = wibox.widget({ content, bg = t.row_bg, fg = t.row_fg, widget = wibox.container.background })
	P.apply_hover(bg_box, t, t.row_bg, t.row_bg_hover)

	bg_box:buttons(gears.table.join(awful.button({}, 1, function()
		if item.on_press then
			item.on_press()
		end
	end)))

	return P.fixed_height(bg_box, eff_h)
end

function P.list_widget(items, t)
	t = with_defaults(t)
	local box = { layout = wibox.layout.fixed.vertical, spacing = t.list_spacing }
	for _, it in ipairs(items or {}) do
		table.insert(box, P.row_widget(it, t))
	end
	return wibox.widget(box)
end

-- ========== POWER-BUTTON ==========
-- btn = { icon=..., text=..., on_press=function() ... end }
function P.power_button(btn, t)
	t = with_defaults(t)

	-- verfügbare Höhe für Ratio-Berechnung: aus Power-Bar (inner_h) oder eigener power_h
	local eff_h = (t._power_inner_h or t.power_h or 48)
	local pad_t = t.power_pad_t or 0
	local pad_b = t.power_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = resolve_icon_size(t, avail_h, "power")
	local font = resolve_font(t, avail_h, "power")

	local inner_hbox = wibox.widget({
		{
			image = btn.icon,
			resize = true,
			forced_height = icon_px,
			forced_width = icon_px,
			widget = wibox.widget.imagebox,
		},
		{
			text = btn.text or "",
			font = font, -- Ratio/px-basierte Schrift
			valign = "center",
			widget = wibox.widget.textbox,
		},
		spacing = t.power_spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	local placed = wibox.widget({
		inner_hbox,
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

	P.apply_hover(box, t, t.power_bg, t.power_bg_hover)

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

-- ========== POWER-BAR (rechte Leiste) ==========
-- power_items: { {id?, text/label, icon, on_press?}, ... }
-- opts.inner_h: Innenhöhe (Footer_H - Pads), opts.dialogs: { power=fn, logout_confirm=fn }
function P.power_bar(power_items, t, opts)
	t = with_defaults(t)
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

		local btn = P.power_button(p, t_btn)
		local fixed = wibox.widget({ btn, strategy = "exact", height = inner_h, widget = wibox.container.constraint })

		local inner = btn._click_target or btn
		inner:buttons({})
		btn:buttons({})

		local raw = (p.text or p.label or ""):lower()
		local key = (p.id or raw):gsub("%s+", ""):lower()

		local function bind(handler)
			local b = gears.table.join(awful.button({}, 1, handler))
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

-- ========== HEADER: Avatar + Text (Inhalt, kein Container) ==========
-- avail_h = header_h - pad_t - pad_b
function P.build_header_content(user, t, avail_h)
	t = with_defaults(t or {})
	user = user or {}

	local icon_size = resolve_icon_size(t, avail_h, "header") -- avatar über icon_ratio[_header]
	local font = resolve_font(t, avail_h, "header")

	local AV_SHAPE = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, t.avatar_radius or 8)
	end

	local avatar_img = wibox.widget({
		image = user.avatar or nil,
		resize = true,
		forced_width = icon_size,
		forced_height = icon_size,
		clip_shape = AV_SHAPE,
		widget = wibox.widget.imagebox,
	})

	local name_lbl = wibox.widget({
		text = user.name or "",
		font = font,
		valign = "center",
		align = "left",
		widget = wibox.widget.textbox,
	})

	local subtitle_lbl = wibox.widget({
		text = user.subtitle or "",
		font = font,
		valign = "center",
		align = "left",
		widget = wibox.widget.textbox,
	})

	local has_sub = (user.subtitle and user.subtitle ~= "")
	subtitle_lbl.visible = has_sub

	local text_column = wibox.widget({
		name_lbl,
		subtitle_lbl,
		spacing = has_sub and (t.header_text_spacing or 2) or 0,
		layout = wibox.layout.fixed.vertical,
	})

	local inner_line = wibox.widget({
		avatar_img,
		text_column,
		spacing = t.header_spacing or 10,
		layout = wibox.layout.fixed.horizontal,
	})

	local inner_centered = wibox.widget({
		inner_line,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local api = {}
	function api.set_user(name, avatar_path, subtitle)
		if name ~= nil then
			name_lbl.text = name
		end
		if avatar_path ~= nil then
			avatar_img.image = avatar_path
		end
		if subtitle ~= nil then
			subtitle_lbl.text = subtitle
			local show = (subtitle ~= "")
			subtitle_lbl.visible = show
			text_column.spacing = show and (t.header_text_spacing or 2) or 0
		end
	end

	return { widget = inner_centered, set_user = api.set_user }
end

return P
