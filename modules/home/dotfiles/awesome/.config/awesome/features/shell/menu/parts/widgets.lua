-- ~/.config/awesome/features/shell/menu/parts/widgets.lua
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
	bg = "#235CDB",
	fg = "#FFFFFF",
	bg_focus = nil,

	-- Rows (Columns)
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	row_bg_hover = nil,
	row_h = 48,
	list_spacing = 0, -- kein sichtbarer Abstand zwischen Reihen
	row_spacing = 8,
	row_pad_l = 10,
	row_pad_r = 10,
	row_pad_t = 4,
	row_pad_b = 4,

	-- Typografie / Ratios (optional; greifen nur, wenn gesetzt)
	font_family = "Sans",
	text_size = nil, -- absolut global
	text_size_rows = nil, -- absolut für rows
	text_ratio = nil, -- globaler Ratio (0..1) – nur wenn gesetzt
	text_ratio_rows = nil, -- Ratio nur für rows
	text_min = nil, -- optionale Klammern
	text_max = nil,

	-- Icons (absolut vs. Ratio)
	icon_size = 18, -- alter Default beibehalten
	icon_ratio = nil, -- globaler Ratio (0..1) – nur wenn gesetzt
	icon_ratio_rows = nil, -- Ratio nur für rows

	-- Footer power buttons
	footer_bg = "#235CDB",
	footer_fg = "#FFFFFF",
	power_bg = nil,
	power_fg = nil,
	power_bg_hover = nil,
	power_w = 110,
	power_h = 48,
	power_icon_size = 16,
	power_spacing = 6,
	power_pad_l = 10,
	power_pad_r = 10,
	power_pad_t = 4,
	power_pad_b = 4,
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

-- ---------- Ratio-Resolver (greifen nur, wenn Ratios/Größen gesetzt sind) ----------
local function resolve_text_px(t, eff_h, kind)
	-- absolute Größe hat Vorrang
	if kind == "rows" and t.text_size_rows then
		return t.text_size_rows
	end
	if t.text_size then
		return t.text_size
	end

	-- Ratio? (nur, wenn gesetzt)
	local ratio = nil
	if kind == "rows" and t.text_ratio_rows then
		ratio = t.text_ratio_rows
	end
	if not ratio then
		ratio = t.text_ratio
	end
	if not ratio then
		return nil
	end -- kein Ratio gesetzt → nichts ändern

	local sz = math.max(1, math.floor(eff_h * ratio + 0.5))
	if t.text_min then
		sz = math.max(sz, t.text_min)
	end
	if t.text_max then
		sz = math.min(sz, t.text_max)
	end
	return sz
end

local function resolve_font(t, eff_h, kind)
	local px = resolve_text_px(t, eff_h, kind)
	if not px then
		return nil
	end
	local family = t.font_family or "Sans"
	return family .. " " .. tostring(px)
end

local function resolve_icon_px(t, eff_h, kind)
	-- absolute Größe hat Vorrang
	if kind == "rows" and t.icon_size then
		return t.icon_size
	end

	-- Ratio? (nur, wenn gesetzt)
	local ratio = nil
	if kind == "rows" and t.icon_ratio_rows then
		ratio = t.icon_ratio_rows
	end
	if not ratio then
		ratio = t.icon_ratio
	end
	if ratio then
		return math.max(1, math.floor(eff_h * ratio + 0.5))
	end

	-- Fallback auf bisherigen Default
	return t.icon_size or DEFAULTS.icon_size
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
-- item = { icon=..., text=..., on_press=function() ... end }
function P.row_widget(item, t)
	t = with_defaults(t)

	local eff_h = t.row_h or 48
	local pad_t = t.row_pad_t or 0
	local pad_b = t.row_pad_b or 0
	local avail_h = math.max(eff_h - pad_t - pad_b, 1)

	local icon_px = resolve_icon_px(t, avail_h, "rows")
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
			font = font, -- wird nur gesetzt, wenn Ratio/px bestimmt wurde
			valign = "center", -- vertikal zentriert
			widget = wibox.widget.textbox,
		},
		spacing = t.row_spacing or 8,
		layout = wibox.layout.fixed.horizontal,
	})

	-- vertikal zentriert, linksbündig
	local centered = wibox.widget({
		hline,
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local content = wibox.widget({
		centered,
		left = t.row_pad_l or 10,
		right = t.row_pad_r or 10,
		top = pad_t,
		bottom = pad_b,
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

-- ---------- FOOTER-POWER-BUTTON ----------
-- btn = { icon=..., text=..., on_press=function() ... end }
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
			valign = "center",
			widget = wibox.widget.textbox,
		},
		spacing = t.power_spacing or 6,
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

-- ---------- POWER-BAR (rechte Button-Leiste für den Footer) ----------
-- power_items: { {id?, text/label, icon, on_press?}, ... }
-- t: Theme/Größen
-- opts.inner_h: Innenhöhe (Footer_H - Pads)
-- opts.dialogs: Modul mit .power() und .logout_confirm() (optional)
function P.power_bar(power_items, t, opts)
	t = with_defaults(t)
	opts = opts or {}
	local inner_h = opts.inner_h or t.footer_h or 48

	local bar = { layout = wibox.layout.fixed.horizontal, spacing = t.power_bar_spacing or 0 }

	for _, p in ipairs(power_items or {}) do
		-- eigene Theme-Kopie pro Button
		local t_btn = {}
		for k, v in pairs(t) do
			t_btn[k] = v
		end
		t_btn.defer_power_clicks = true
		t_btn._power_inner_h = inner_h -- falls du künftig Ratios nutzen willst

		local btn = P.power_button(p, t_btn)
		local fixed = wibox.widget({
			btn,
			strategy = "exact",
			height = inner_h,
			widget = wibox.container.constraint,
		})

		local inner = btn._click_target or btn
		inner:buttons({})
		btn:buttons({}) -- Default-Bindings leeren, wir binden zentral

		local raw_text = (p.text or p.label or ""):lower()
		local key = (p.id or raw_text):gsub("%s+", ""):lower()

		local function bind(handler)
			local b = gears.table.join(awful.button({}, 1, handler))
			inner:buttons(b)
			btn:buttons(b)
		end

		local matched = false
		if opts.dialogs then
			if key == "power" or raw_text:find("shutdown") or raw_text:find("turnoff") then
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
			elseif key == "logout" or key == "logoff" or raw_text:find("logout") or raw_text:find("exit") then
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

	-- rechtsbündig ausrichten
	return wibox.widget({ bar, halign = "right", widget = wibox.container.place })
end

-- ---------- HEADER: Avatar + Text (nur Inhalt, kein Container) ----------
-- user: { name, avatar, subtitle }
-- t: Theme (icon_ratio_header/text_ratio_header | avatar_size/text_size_header, header_text_spacing, header_spacing, font_family, avatar_radius)
-- avail_h: verfügbare Innenhöhe (Header-H minus Top/Bottom-Padding)
function P.build_header_content(user, t, avail_h)
	t = with_defaults(t or {})
	user = user or {}

	local function resolve_header_icon_size()
		if t.avatar_size then
			return t.avatar_size
		end
		local ratio = t.icon_ratio_header or t.icon_ratio or 0.6
		return math.max(1, math.floor(avail_h * ratio + 0.5))
	end

	local function resolve_header_font()
		local family = t.font_family or "Sans"
		local sz = t.text_size_header or t.text_size
		if not sz then
			local ratio = t.text_ratio_header or t.text_ratio or 0.20
			sz = math.max(1, math.floor(avail_h * ratio + 0.5))
			if t.text_min then
				sz = math.max(sz, t.text_min)
			end
			if t.text_max then
				sz = math.min(sz, t.text_max)
			end
		end
		return family .. " " .. tostring(sz)
	end

	local icon_size = resolve_header_icon_size()
	local font = resolve_header_font()

	local AV_RAD = t.avatar_radius or 8
	local AV_SHAPE = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, AV_RAD)
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
		halign = "left", -- linksbündig
		valign = "center", -- vertikal zentriert
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
