-- ~/.config/awesome/shell/notify/center/widget.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Internal
-- =========================================================================

local function notify_theme()
	return beautiful.notify or {}
end

local function center_theme()
	local notify = notify_theme()
	return notify.center or {}
end

local function resolve_theme()
	local notify = notify_theme()
	local center = center_theme()

	return {
		panel_bg = center.panel_bg or "#00000000",

		entry_bg = center.entry_bg or notify.bg,
		entry_fg = center.entry_fg or notify.fg,
		entry_border = center.entry_border or notify.border,

		entry_radius = tonumber(center.entry_radius or notify.radius),
		entry_border_w = tonumber(center.entry_border_w or notify.border_w),
		entry_padding = tonumber(center.entry_padding or notify.margin),
		entry_spacing = tonumber(center.entry_spacing),

		text_inset_top = tonumber(center.text_inset_top or 2),
		text_inset_bottom = tonumber(center.text_inset_bottom or 2),

		list_pad_top = tonumber(center.list_pad_top),
		list_pad_right = tonumber(center.list_pad_right),
		list_pad_bottom = tonumber(center.list_pad_bottom),
		list_pad_left = tonumber(center.list_pad_left),

		action_bg = center.action_bg,
		action_fg = center.action_fg,
		action_border = center.action_border,
		action_border_w = tonumber(center.action_border_w),
		action_radius = tonumber(center.action_radius),
		action_spacing = tonumber(center.action_spacing),
		action_padding_h = tonumber(center.action_padding_h),
		action_padding_v = tonumber(center.action_padding_v),
	}
end

local function rounded_shape(radius)
	return function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, radius)
	end
end

local function entry_title(entry)
	local title = entry.title

	if title == nil or title == "" then
		title = entry.app_name or "Notification"
	end

	return title
end

local function entry_message(entry)
	return entry.message or entry.text or ""
end

local function make_textbox(theme, text, valign)
	local tb = wibox.widget({
		text = text or "",
		align = "left",
		valign = valign or "top",
		wrap = "word_char",
		widget = wibox.widget.textbox,
	})

	local inset = wibox.container.margin(tb)
	inset.top = theme.text_inset_top
	inset.bottom = theme.text_inset_bottom

	return inset
end

local function invoke_action(entry, item, cfg)
	if type(item.callback) == "function" then
		item.callback(entry.raw)
	end

	if cfg.notify.actions.invoke_closes_center == true then
		awesome.emit_signal("notify::close_center")
	end
end

local function build_action(theme, entry, item, cfg)
	local label = wibox.widget({
		text = item.label or "Action",
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local inner = wibox.container.margin(label)
	inner.left = theme.action_padding_h
	inner.right = theme.action_padding_h
	inner.top = theme.action_padding_v
	inner.bottom = theme.action_padding_v

	local background = wibox.container.background(inner)
	background.bg = theme.action_bg
	background.fg = theme.action_fg
	background.shape = rounded_shape(theme.action_radius)
	background.shape_border_width = theme.action_border_w
	background.shape_border_color = theme.action_border

	background:buttons(gears.table.join(awful.button({}, 1, function()
		invoke_action(entry, item, cfg)
	end)))

	return background
end

local function build_actions(theme, entry, cfg)
	if cfg.notify.actions.show ~= true then
		return nil
	end

	if type(entry.actions) ~= "table" or #entry.actions == 0 then
		return nil
	end

	local row = wibox.layout.fixed.horizontal()
	row.spacing = theme.action_spacing

	for _, item in ipairs(entry.actions) do
		row:add(build_action(theme, entry, item, cfg))
	end

	return row
end

local function build_card(theme, entry, cfg)
	local text_column = wibox.layout.fixed.vertical()
	text_column.spacing = 4
	text_column:add(make_textbox(theme, entry_title(entry), "center"))

	local message = entry_message(entry)
	if message ~= "" then
		text_column:add(make_textbox(theme, message, "top"))
	end

	local actions = build_actions(theme, entry, cfg)
	if actions then
		text_column:add(actions)
	end

	local inner = wibox.container.margin(text_column)
	inner.left = theme.entry_padding
	inner.right = theme.entry_padding
	inner.top = theme.entry_padding + 2
	inner.bottom = theme.entry_padding + 2

	local background = wibox.container.background(inner)
	background.bg = theme.entry_bg
	background.fg = theme.entry_fg
	background.shape = rounded_shape(theme.entry_radius)
	background.shape_border_width = theme.entry_border_w
	background.shape_border_color = theme.entry_border

	return background
end

local function build_list(theme, entries, cfg)
	if not entries or #entries == 0 then
		return nil
	end

	local list = wibox.layout.fixed.vertical()
	list.spacing = theme.entry_spacing

	for i = #entries, 1, -1 do
		list:add(build_card(theme, entries[i], cfg))
	end

	return list
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(entries, _max_height, cfg)
	local theme = resolve_theme()
	local list = build_list(theme, entries, cfg)

	if not list then
		return nil
	end

	local padded = wibox.container.margin(list)
	padded.top = theme.list_pad_top
	padded.right = theme.list_pad_right
	padded.bottom = theme.list_pad_bottom
	padded.left = theme.list_pad_left

	local panel = wibox.container.background(padded)
	panel.bg = theme.panel_bg

	return panel
end

return M
