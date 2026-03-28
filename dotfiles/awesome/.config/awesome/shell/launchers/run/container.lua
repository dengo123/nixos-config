-- ~/.config/awesome/shell/launchers/run/container.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function build_title_widget(_theme, title_text)
	return wibox.widget({
		text = title_text,
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

local function build_header(theme, title_text)
	local header_pad_l = assert(tonumber(theme.header_pad_l), "run.container: theme.header_pad_l fehlt/ungültig")
	local header_pad_r = assert(tonumber(theme.header_pad_r), "run.container: theme.header_pad_r fehlt/ungültig")
	local header_pad_v = assert(tonumber(theme.header_pad_v), "run.container: theme.header_pad_v fehlt/ungültig")

	local title = build_title_widget(theme, title_text)

	return wibox.widget({
		{
			{
				title,
				halign = "left",
				valign = "center",
				widget = wibox.container.place,
			},
			left = header_pad_l,
			right = header_pad_r,
			top = header_pad_v,
			bottom = header_pad_v,
			widget = wibox.container.margin,
		},
		bg = theme.header_bg,
		fg = theme.header_fg,
		widget = wibox.container.background,
	})
end

local function build_body(theme, dims, body_widget)
	local panel_pad_h = assert(
		tonumber(theme.panel_pad_h or theme.pad_h),
		"run.container: theme.panel_pad_h/theme.pad_h fehlt/ungültig"
	)
	local panel_pad_v = assert(
		tonumber(theme.panel_pad_v or theme.pad_v),
		"run.container: theme.panel_pad_v/theme.pad_v fehlt/ungültig"
	)

	return wibox.widget({
		{
			body_widget,
			left = panel_pad_h,
			right = panel_pad_h,
			top = panel_pad_v,
			bottom = panel_pad_v,
			widget = wibox.container.margin,
		},
		bg = theme.body_bg,
		fg = theme.body_fg,
		widget = wibox.container.background,
	})
end

local function build_footer(theme, footer_buttons, footer_widget)
	if footer_widget then
		return footer_widget
	end

	if not footer_buttons or #footer_buttons == 0 then
		return nil
	end

	local footer_spacing = assert(tonumber(theme.footer_spacing), "run.container: theme.footer_spacing fehlt/ungültig")
	local footer_pad_h = assert(
		tonumber(theme.footer_pad_h or theme.pad_h),
		"run.container: theme.footer_pad_h/theme.pad_h fehlt/ungültig"
	)
	local footer_pad_v = assert(
		tonumber(theme.footer_pad_v or theme.pad_v),
		"run.container: theme.footer_pad_v/theme.pad_v fehlt/ungültig"
	)

	local buttons_row = wibox.layout.fixed.horizontal()
	buttons_row.spacing = footer_spacing

	for _, btn in ipairs(footer_buttons) do
		if btn then
			buttons_row:add(btn)
		end
	end

	return wibox.widget({
		{
			{
				nil,
				buttons_row,
				expand = "outside",
				layout = wibox.layout.align.horizontal,
			},
			halign = "right",
			valign = "center",
			widget = wibox.container.place,
		},
		left = footer_pad_h,
		right = footer_pad_h,
		top = footer_pad_v,
		bottom = footer_pad_v,
		widget = wibox.container.margin,
	})
end

local function build_frame(theme, radius, border_w, border_color, stack)
	local inner_radius = math.max(0, radius - border_w)

	local inner_bg = wibox.widget({
		{
			stack,
			widget = wibox.container.background,
		},
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, inner_radius)
		end,
		shape_clip = true,
		bg = theme.panel_bg,
		widget = wibox.container.background,
	})

	local inset = wibox.widget({
		inner_bg,
		left = border_w,
		right = border_w,
		top = border_w,
		bottom = border_w,
		widget = wibox.container.margin,
	})

	return wibox.widget({
		inset,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		shape_clip = true,
		bg = border_color,
		widget = wibox.container.background,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(theme, dims, opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local radius = assert(tonumber(theme.panel_radius), "run.container: theme.panel_radius fehlt/ungültig")
	local border_w =
		assert(tonumber(theme.panel_border_width), "run.container: theme.panel_border_width fehlt/ungültig")
	local border_color = assert(theme.panel_border, "run.container: theme.panel_border fehlt")

	local header_h = assert(tonumber(theme.header_h), "run.container: theme.header_h fehlt/ungültig")
	local footer_h = assert(tonumber(theme.footer_h), "run.container: theme.footer_h fehlt/ungültig")

	local title_text = opts.title or theme.title
	local footer_widget = build_footer(theme, opts.footer_buttons, opts.footer)

	-- ---------------------------------------------------------------------
	-- Sections
	-- ---------------------------------------------------------------------

	local header = build_header(theme, title_text)
	local body = build_body(theme, dims, opts.body)

	local final_footer_h = footer_widget and footer_h or 0
	local final_body_h = math.max(0, dims.h - header_h - final_footer_h)

	-- ---------------------------------------------------------------------
	-- Stack
	-- ---------------------------------------------------------------------

	local stack = wibox.widget({
		{
			header,
			strategy = "exact",
			height = header_h,
			widget = wibox.container.constraint,
		},
		{
			body,
			strategy = "exact",
			height = final_body_h,
			widget = wibox.container.constraint,
		},
		layout = wibox.layout.fixed.vertical,
	})

	if footer_widget then
		stack = wibox.widget({
			stack,
			{
				{
					footer_widget,
					bg = theme.footer_bg,
					fg = theme.footer_fg,
					widget = wibox.container.background,
				},
				strategy = "exact",
				height = final_footer_h,
				widget = wibox.container.constraint,
			},
			layout = wibox.layout.fixed.vertical,
		})
	end

	-- ---------------------------------------------------------------------
	-- Frame
	-- ---------------------------------------------------------------------

	return build_frame(theme, radius, border_w, border_color, stack)
end

return M
