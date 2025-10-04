-- ~/.config/awesome/shell/launchers/power/container.lua
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- Icon-Widget aus Theme (Bild > Text > nil)
local function resolve_icon_widget(th)
	local size = tonumber(th.header_icon_size) or 48
	local path = th.header_icon_path

	if type(path) == "string" and #path > 0 then
		if not path:match("^/") then
			path = gears.filesystem.get_configuration_dir() .. path
		end
		return wibox.widget({
			image = path,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	end

	local txt = th.header_icon_text
	if type(txt) == "string" and #txt > 0 then
		return wibox.widget({
			markup = string.format("<span font='%s %d'>%s</span>", th.header_font or "Sans", size, txt),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	return nil
end

-- Titel-Widget aus Theme/Text
local function resolve_title_widget(th, title_text)
	if not title_text or #tostring(title_text) == 0 then
		return nil
	end
	return wibox.widget({
		markup = string.format(
			"<span font='%s %d'><b>%s</b></span>",
			th.header_font or "Sans",
			tonumber(th.header_font_size) or 18,
			tostring(title_text)
		),
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

-- build(th, dims, { title, body, cancel_btn })
function M.build(th, dims, slots)
	local title_text = (slots.title ~= nil) and slots.title or pick(th.header_title, "Turn off Computer")
	local body_core = slots.body
	local cancel_btn = slots.cancel_btn

	-- ===== Header: links Titel, rechts Icon (an Außenrändern) =====
	local title_w = resolve_title_widget(th, title_text)
	local icon_w = resolve_icon_widget(th)

	local left_place = wibox.widget({
		title_w or wibox.widget({}),
		halign = "left",
		valign = "center",
		widget = wibox.container.place,
	})

	local right_place = wibox.widget({
		icon_w or wibox.widget({}),
		halign = "right",
		valign = "center",
		widget = wibox.container.place,
	})

	-- einziges Header-Padding (voll aus Theme steuerbar)
	local pad_l = tonumber(th.header_pad_l) or tonumber(th.header_pad_h) or 0
	local pad_r = tonumber(th.header_pad_r) or tonumber(th.header_pad_h) or 0
	local pad_v = tonumber(th.header_pad_v) or 0

	local header = wibox.widget({
		{
			{
				left_place, -- links am Außenrand
				nil, -- keine Mitte nötig
				right_place, -- rechts am Außenrand
				layout = wibox.layout.align.horizontal,
			},
			left = pad_l,
			right = pad_r,
			top = pad_v,
			bottom = pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.header_bg, "#1A50B8"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- ===== Body =====
	local body = wibox.widget({
		{
			{ body_core, halign = "center", valign = "center", widget = wibox.container.place },
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#0B89E7"),
		fg = pick(th.body_fg, "#000000"),
		widget = wibox.container.background,
	})

	-- ===== Footer (Farben wie Header) =====
	local footer = wibox.widget({
		{
			{ cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = pick(th.footer_bg, th.header_bg, "#1A50B8"),
		fg = pick(th.footer_fg, th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- ===== Stack mit fixen Höhen =====
	local stack = wibox.widget({
		{ header, strategy = "exact", height = dims.header_h, widget = wibox.container.constraint },
		{ body, strategy = "exact", height = dims.body_h, widget = wibox.container.constraint },
		{ footer, strategy = "exact", height = dims.footer_h, widget = wibox.container.constraint },
		layout = wibox.layout.fixed.vertical,
	})

	-- ===== Rechteckiger Rahmen (keine Rundungen) =====
	return wibox.widget({
		stack,
		shape = function(cr, w, h)
			gears.shape.rectangle(cr, w, h)
		end,
		border_width = tonumber(th.dialog_border_width) or 2,
		border_color = pick(th.dialog_border, "#1A50B8"),
		bg = pick(th.dialog_bg, "#053193"),
		widget = wibox.container.background,
	})
end

return M
