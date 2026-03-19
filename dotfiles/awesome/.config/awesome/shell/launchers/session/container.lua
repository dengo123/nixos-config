-- ~/.config/awesome/shell/launchers/session/container.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function escape(text)
	return gears.string.xml_escape(tostring(text or ""))
end

local function resolve_icon_widget(th)
	local size = assert(tonumber(th.header_icon_size), "power.container: header_icon_size fehlt/ungültig")
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
			markup = string.format("<span font='%s %d'>%s</span>", th.header_font, size, escape(txt)),
			align = "center",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	return nil
end

local function resolve_title_widget(th, title_text)
	if title_text == nil or tostring(title_text) == "" then
		return nil
	end

	return wibox.widget({
		markup = string.format(
			"<span font='%s %d'><b>%s</b></span>",
			th.header_font,
			assert(tonumber(th.header_font_size), "power.container: header_font_size fehlt/ungültig"),
			escape(title_text)
		),
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

-- ============================================================================
-- Public API
-- ============================================================================

function M.build(th, dims, slots)
	slots = slots or {}

	-- ------------------------------------------------------------------------
	-- Config
	-- ------------------------------------------------------------------------

	local title_text = (slots.title ~= nil) and slots.title or th.header_title
	local body_core = slots.body
	local cancel_btn = slots.cancel_btn

	local header_pad_l = tonumber(th.header_pad_l) or tonumber(th.header_pad_h) or 0
	local header_pad_r = tonumber(th.header_pad_r) or tonumber(th.header_pad_h) or 0
	local header_pad_v = tonumber(th.header_pad_v) or 0

	-- ------------------------------------------------------------------------
	-- Header
	-- ------------------------------------------------------------------------

	local title_w = resolve_title_widget(th, title_text)
	local icon_w = resolve_icon_widget(th)

	local header = wibox.widget({
		{
			{
				{
					title_w or wibox.widget({}),
					halign = "left",
					valign = "center",
					widget = wibox.container.place,
				},
				nil,
				{
					icon_w or wibox.widget({}),
					halign = "right",
					valign = "center",
					widget = wibox.container.place,
				},
				layout = wibox.layout.align.horizontal,
			},
			left = header_pad_l,
			right = header_pad_r,
			top = header_pad_v,
			bottom = header_pad_v,
			widget = wibox.container.margin,
		},
		bg = th.header_bg,
		fg = th.header_fg,
		widget = wibox.container.background,
	})

	-- ------------------------------------------------------------------------
	-- Body
	-- ------------------------------------------------------------------------

	local body = wibox.widget({
		{
			{
				body_core,
				halign = "center",
				valign = "center",
				widget = wibox.container.place,
			},
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = th.body_bg,
		fg = th.body_fg,
		widget = wibox.container.background,
	})

	-- ------------------------------------------------------------------------
	-- Footer
	-- ------------------------------------------------------------------------

	local footer = wibox.widget({
		{
			{
				cancel_btn or wibox.widget({}),
				halign = "right",
				valign = "center",
				widget = wibox.container.place,
			},
			left = dims.pad_h,
			right = dims.pad_h,
			top = dims.pad_v,
			bottom = dims.pad_v,
			widget = wibox.container.margin,
		},
		bg = th.footer_bg,
		fg = th.footer_fg,
		widget = wibox.container.background,
	})

	-- ------------------------------------------------------------------------
	-- Stack
	-- ------------------------------------------------------------------------

	local stack = wibox.widget({
		{
			header,
			strategy = "exact",
			height = dims.header_h,
			widget = wibox.container.constraint,
		},
		{
			body,
			strategy = "exact",
			height = dims.body_h,
			widget = wibox.container.constraint,
		},
		{
			footer,
			strategy = "exact",
			height = dims.footer_h,
			widget = wibox.container.constraint,
		},
		layout = wibox.layout.fixed.vertical,
	})

	-- ------------------------------------------------------------------------
	-- Frame
	-- ------------------------------------------------------------------------

	return wibox.widget({
		stack,
		shape = gears.shape.rectangle,
		shape_clip = true,
		shape_border_width = assert(
			tonumber(th.dialog_border_width),
			"power.container: dialog_border_width fehlt/ungültig"
		),
		shape_border_color = th.dialog_border,
		bg = th.dialog_bg,
		widget = wibox.container.background,
	})
end

return M
