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

		entry_bg_hover = center.entry_bg_hover or center.entry_bg or notify.bg,
		entry_fg_hover = center.entry_fg_hover or center.entry_fg or notify.fg,
		entry_border_hover = center.entry_border_hover or center.entry_border or notify.border,

		entry_bg_focus = center.entry_bg_focus or center.entry_bg_hover or center.entry_bg or notify.bg,
		entry_fg_focus = center.entry_fg_focus or center.entry_fg_hover or center.entry_fg or notify.fg,
		entry_border_focus = center.entry_border_focus
			or center.entry_border_hover
			or center.entry_border
			or notify.border,

		entry_radius = tonumber(center.entry_radius or notify.radius),
		entry_border_w = tonumber(center.entry_border_w or notify.border_w),
		entry_padding = tonumber(center.entry_padding or notify.margin),
		entry_spacing = tonumber(center.entry_spacing) or 0,

		text_inset_top = tonumber(center.text_inset_top or 2),
		text_inset_bottom = tonumber(center.text_inset_bottom or 2),

		list_pad_top = tonumber(center.list_pad_top) or 0,
		list_pad_right = tonumber(center.list_pad_right) or 0,
		list_pad_bottom = tonumber(center.list_pad_bottom) or 0,
		list_pad_left = tonumber(center.list_pad_left) or 0,

		title_font = center.title_font,
		message_font = center.message_font,

		action_fg = center.action_fg or notify.fg,
		action_fg_hover = center.action_fg_hover or center.action_fg or notify.fg,
		action_spacing = tonumber(center.action_spacing) or 0,
		action_row_spacing = tonumber(center.action_row_spacing or center.action_spacing) or 0,
		action_icon = center.action_icon or "▪",
		action_icon_font = center.action_icon_font,
		action_font = center.action_font,
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

local function make_textbox(theme, args)
	args = args or {}

	local tb = wibox.widget({
		align = "left",
		valign = args.valign or "top",
		wrap = "word_char",
		widget = wibox.widget.textbox,
	})

	if args.markup then
		tb.markup = args.markup
	else
		tb.text = args.text or ""
	end

	if args.font then
		tb.font = args.font
	end

	local inset = wibox.container.margin(tb)
	inset.top = theme.text_inset_top
	inset.bottom = theme.text_inset_bottom

	return inset
end

local function build_action(theme, entry, item, cfg, deps)
	local Actions = deps.actions

	local icon = wibox.widget({
		text = theme.action_icon or "▪",
		align = "left",
		valign = "center",
		font = theme.action_icon_font,
		widget = wibox.widget.textbox,
	})

	local label = wibox.widget({
		markup = "<u>" .. gears.string.xml_escape(item.label or "Action") .. "</u>",
		align = "left",
		valign = "center",
		font = theme.action_font,
		widget = wibox.widget.textbox,
	})

	local row = wibox.widget({
		icon,
		label,
		spacing = theme.action_spacing,
		layout = wibox.layout.fixed.horizontal,
	})

	local clickable = wibox.container.background(row)
	clickable.bg = "#00000000"
	clickable.fg = theme.action_fg

	local function set_hover(on)
		clickable.fg = on and theme.action_fg_hover or theme.action_fg
	end

	clickable:connect_signal("mouse::enter", function()
		set_hover(true)
	end)

	clickable:connect_signal("mouse::leave", function()
		set_hover(false)
	end)

	clickable:buttons(gears.table.join(awful.button({}, 1, function()
		if Actions and type(Actions.invoke) == "function" then
			Actions.invoke(entry, item, cfg)
		end
	end)))

	return clickable
end

local function build_actions(theme, entry, cfg, deps)
	if not (cfg.notify and cfg.notify.actions and cfg.notify.actions.show == true) then
		return nil
	end

	if type(entry.actions) ~= "table" or #entry.actions == 0 then
		return nil
	end

	local column = wibox.layout.fixed.vertical()
	column.spacing = theme.action_row_spacing

	for _, item in ipairs(entry.actions) do
		column:add(build_action(theme, entry, item, cfg, deps))
	end

	return column
end

local function build_card(theme, entry, cfg, deps)
	local Actions = deps.actions

	local text_column = wibox.layout.fixed.vertical()
	text_column.spacing = 4

	text_column:add(make_textbox(theme, {
		text = entry_title(entry),
		valign = "center",
		font = theme.title_font,
	}))

	local message = entry_message(entry)
	if message ~= "" then
		text_column:add(make_textbox(theme, {
			text = message,
			valign = "top",
			font = theme.message_font,
		}))
	end

	local actions = build_actions(theme, entry, cfg, deps)
	if actions then
		text_column:add(actions)
	end

	local inner = wibox.container.margin(text_column)
	inner.left = theme.entry_padding
	inner.right = theme.entry_padding
	inner.top = theme.entry_padding + 2
	inner.bottom = theme.entry_padding + 2

	local background = wibox.container.background(inner)
	background.shape = rounded_shape(theme.entry_radius)
	background.shape_border_width = theme.entry_border_w

	local state = {
		hover = false,
		selected = false,
	}

	local item = {
		entry = entry,
		widget = background,
	}

	local function apply_state()
		if state.selected then
			background.bg = theme.entry_bg_focus
			background.fg = theme.entry_fg_focus
			background.shape_border_color = theme.entry_border_focus
		elseif state.hover then
			background.bg = theme.entry_bg_hover
			background.fg = theme.entry_fg_hover
			background.shape_border_color = theme.entry_border_hover
		else
			background.bg = theme.entry_bg
			background.fg = theme.entry_fg
			background.shape_border_color = theme.entry_border
		end
	end

	function item.set_hover(on)
		state.hover = (on == true)
		apply_state()
	end

	function item.set_selected(on)
		state.selected = (on == true)
		apply_state()
	end

	function item.open()
		if Actions and type(Actions.open) == "function" then
			Actions.open(entry, cfg)
		end
	end

	function item.dismiss()
		if Actions and type(Actions.dismiss) == "function" then
			Actions.dismiss(entry, cfg)
		end
	end

	background:connect_signal("mouse::enter", function()
		item.set_hover(true)
	end)

	background:connect_signal("mouse::leave", function()
		item.set_hover(false)
	end)

	background:buttons(gears.table.join(
		awful.button({}, 1, function()
			item.open()
		end),
		awful.button({}, 3, function()
			item.dismiss()
		end),
		awful.button({}, 4, function()
			awesome.emit_signal("notify::center_scroll_up")
		end),
		awful.button({}, 5, function()
			awesome.emit_signal("notify::center_scroll_down")
		end)
	))

	apply_state()
	return item
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(entries, geo, cfg, state, deps)
	deps = deps or {}

	local Actions = deps.actions
	local List = deps.list
	local View = deps.view

	local theme = resolve_theme()

	local center = center_theme()
	local popup_width = tonumber(geo and geo.width) or tonumber(center.max_width) or tonumber(center.min_width) or 320

	local popup_height = tonumber(geo and geo.height) or 1

	local list_width = math.max(1, popup_width - (theme.list_pad_left or 0) - (theme.list_pad_right or 0))

	if not (List and type(List.build) == "function") then
		return nil
	end

	local built = List.build({
		theme = theme,
		entries = entries,
		cfg = cfg,
		state = state,
		max_height = popup_height,
		list_width = list_width,
		build_card = function(theme_, entry_, cfg_)
			return build_card(theme_, entry_, cfg_, {
				actions = Actions,
			})
		end,
	})

	if not built or not built.widget then
		return nil
	end

	if not (View and type(View.build) == "function") then
		return nil
	end

	return {
		widget = View.build({
			widget = built.widget,
			popup_width = popup_width,
			popup_height = popup_height,
			pad_top = theme.list_pad_top,
			pad_right = theme.list_pad_right,
			pad_bottom = theme.list_pad_bottom,
			pad_left = theme.list_pad_left,
			bg = theme.panel_bg,
		}),
		items = built.items,
		total = built.total,
		visible = built.visible,
	}
end

return M
