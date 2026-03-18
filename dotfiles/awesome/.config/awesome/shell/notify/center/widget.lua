-- ~/.config/awesome/shell/notify/center/widget.lua
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function build_header(theme)
	return wibox.widget({
		{
			{
				text = "Notifications",
				align = "left",
				valign = "center",
				widget = wibox.widget.textbox,
			},
			left = theme.padding,
			right = theme.padding,
			top = theme.padding,
			bottom = theme.padding,
			widget = wibox.container.margin,
		},
		bg = theme.header_bg,
		fg = theme.header_fg,
		widget = wibox.container.background,
	})
end

local function build_empty(theme)
	return wibox.widget({
		{
			{
				text = "Keine Benachrichtigungen im Verlauf.",
				align = "left",
				valign = "center",
				wrap = "word_char",
				widget = wibox.widget.textbox,
			},
			left = theme.padding,
			right = theme.padding,
			top = theme.padding,
			bottom = theme.padding,
			widget = wibox.container.margin,
		},
		bg = theme.body_bg,
		fg = theme.body_fg,
		widget = wibox.container.background,
	})
end

local function build_entry(theme, entry)
	local title = entry.title
	if title == nil or title == "" then
		title = entry.app_name or "Notification"
	end

	local message = entry.message or ""

	return wibox.widget({
		{
			{
				{
					text = title,
					align = "left",
					valign = "center",
					wrap = "word_char",
					widget = wibox.widget.textbox,
				},
				{
					text = message,
					align = "left",
					valign = "top",
					wrap = "word_char",
					widget = wibox.widget.textbox,
				},
				spacing = 6,
				layout = wibox.layout.fixed.vertical,
			},
			left = theme.padding,
			right = theme.padding,
			top = theme.padding,
			bottom = theme.padding,
			widget = wibox.container.margin,
		},
		bg = theme.body_bg,
		fg = theme.body_fg,
		widget = wibox.container.background,
	})
end

local function build_body(theme, entries, visible_entries)
	local count = math.min(#entries, visible_entries)

	if count <= 0 then
		return build_empty(theme)
	end

	local column = wibox.layout.fixed.vertical()
	column.spacing = theme.spacing

	for i = 1, count do
		column:add(wibox.widget({
			build_entry(theme, entries[i]),
			strategy = "exact",
			height = theme.entry_h,
			widget = wibox.container.constraint,
		}))
	end

	return wibox.widget({
		column,
		bg = theme.body_bg,
		fg = theme.body_fg,
		widget = wibox.container.background,
	})
end

local function build_frame(theme, stack)
	local radius = theme.panel_radius
	local border_w = theme.panel_border_w
	local border_color = theme.panel_border
	local panel_bg = theme.panel_bg
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
		bg = panel_bg,
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

function M.build_panel(args)
	local theme = args.theme
	local entries = args.entries or {}
	local visible_entries = args.visible_entries or 5
	local body_h = args.body_h

	local stack = wibox.widget({
		{
			build_header(theme),
			strategy = "exact",
			height = theme.header_h,
			widget = wibox.container.constraint,
		},
		{
			build_body(theme, entries, visible_entries),
			strategy = "exact",
			height = body_h,
			widget = wibox.container.constraint,
		},
		layout = wibox.layout.fixed.vertical,
	})

	return build_frame(theme, stack)
end

return M
