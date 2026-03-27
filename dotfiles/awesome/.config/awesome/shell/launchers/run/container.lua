-- ~/.config/awesome/shell/launchers/run/container.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function build_title_widget(th, title_text)
	return wibox.widget({
		text = title_text,
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
end

local function build_header(th, title_text)
	local header_pad_l = assert(tonumber(th.header_pad_l), "run.container: th.header_pad_l fehlt/ungültig")
	local header_pad_r = assert(tonumber(th.header_pad_r), "run.container: th.header_pad_r fehlt/ungültig")
	local header_pad_v = assert(tonumber(th.header_pad_v), "run.container: th.header_pad_v fehlt/ungültig")

	local title = build_title_widget(th, title_text)

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
		bg = th.header_bg,
		fg = th.header_fg,
		widget = wibox.container.background,
	})
end

local function build_body(th, dims, body_widget)
	local panel_pad_h =
		assert(tonumber(th.panel_pad_h or th.pad_h), "run.container: th.panel_pad_h/th.pad_h fehlt/ungültig")
	local panel_pad_v =
		assert(tonumber(th.panel_pad_v or th.pad_v), "run.container: th.panel_pad_v/th.pad_v fehlt/ungültig")

	return wibox.widget({
		{
			body_widget,
			left = panel_pad_h,
			right = panel_pad_h,
			top = panel_pad_v,
			bottom = panel_pad_v,
			widget = wibox.container.margin,
		},
		bg = th.body_bg,
		fg = th.body_fg,
		widget = wibox.container.background,
	})
end

local function build_footer(th, footer_buttons, footer_widget)
	if footer_widget then
		return footer_widget
	end

	if not footer_buttons or #footer_buttons == 0 then
		return nil
	end

	local footer_spacing = assert(tonumber(th.footer_spacing), "run.container: th.footer_spacing fehlt/ungültig")
	local footer_pad_h =
		assert(tonumber(th.footer_pad_h or th.pad_h), "run.container: th.footer_pad_h/th.pad_h fehlt/ungültig")
	local footer_pad_v =
		assert(tonumber(th.footer_pad_v or th.pad_v), "run.container: th.footer_pad_v/th.pad_v fehlt/ungültig")

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

local function build_frame(th, radius, border_w, border_color, stack)
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
		bg = th.panel_bg,
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

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return M
end

function M.build(th, dims, w)
	w = w or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local radius = assert(tonumber(th.panel_radius), "run.container: th.panel_radius fehlt/ungültig")
	local border_w = assert(tonumber(th.panel_border_width), "run.container: th.panel_border_width fehlt/ungültig")
	local border_color = assert(th.panel_border, "run.container: th.panel_border fehlt")

	local header_h = assert(tonumber(th.header_h), "run.container: th.header_h fehlt/ungültig")
	local footer_h = assert(tonumber(th.footer_h), "run.container: th.footer_h fehlt/ungültig")

	local title_text = w.title or th.title
	local footer_widget = build_footer(th, w.footer_buttons, w.footer)

	-- ---------------------------------------------------------------------
	-- Sections
	-- ---------------------------------------------------------------------

	local header = build_header(th, title_text)
	local body = build_body(th, dims, w.body)

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
					bg = th.footer_bg,
					fg = th.footer_fg,
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

	return build_frame(th, radius, border_w, border_color, stack)
end

return M
