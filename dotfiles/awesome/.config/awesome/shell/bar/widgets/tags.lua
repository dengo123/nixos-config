-- ~/.config/awesome/shell/bar/widgets/tags.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function require_theme_table(value, name)
	assert(type(value) == "table", "tags: " .. name .. " fehlt/ungültig")
	return value
end

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "tags: " .. name .. " fehlt/ungültig")
	return n
end

local function first_string(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if type(v) == "string" and v ~= "" then
			return v
		end
	end
	return nil
end

local function tag_count(s)
	local tags = s.tags or {}
	return #tags
end

local function selected_index(s)
	local t = s.selected_tag or awful.screen.focused().selected_tag
	return (t and t.index) or 1
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(s, opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local theme = require_theme_table(beautiful.tags_indicator, "beautiful.tags_indicator")

	local pad_h = tonumber(opts.pad_h) or require_number(theme.pad_h, "beautiful.tags_indicator.pad_h")
	local pad_v = tonumber(opts.pad_v) or require_number(theme.pad_v, "beautiful.tags_indicator.pad_v")
	local collapsed_pad_h = require_number(theme.collapsed_pad_h, "beautiful.tags_indicator.collapsed_pad_h")

	local font = first_string(opts.font, theme.font, beautiful.font)
	assert(font, "tags: font fehlt/ungültig")

	local format = first_string(opts.format, theme.fmt)
	assert(format, "tags: beautiful.tags_indicator.fmt fehlt/ungültig")

	-- ---------------------------------------------------------------------
	-- Widgets
	-- ---------------------------------------------------------------------

	local text = wibox.widget({
		widget = wibox.widget.textbox,
		font = font,
		align = "center",
		valign = "center",
	})

	local indicator = wibox.widget({
		text,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
		widget = wibox.container.margin,
	})

	-- ---------------------------------------------------------------------
	-- State Helpers
	-- ---------------------------------------------------------------------

	local function set_normal_margins()
		indicator.left = pad_h
		indicator.right = pad_h
		indicator.top = pad_v
		indicator.bottom = pad_v
	end

	local function clear_forced_width()
		indicator.forced_width = nil
	end

	local function set_collapsed_spacing()
		text.text = ""
		indicator.left = collapsed_pad_h
		indicator.right = collapsed_pad_h
		indicator.top = pad_v
		indicator.bottom = pad_v
		indicator.forced_width = 2 * collapsed_pad_h
		indicator.visible = true
	end

	local function refresh()
		if tag_count(s) <= 1 then
			set_collapsed_spacing()
			return
		end

		clear_forced_width()
		set_normal_margins()
		indicator.visible = true
		text.text = string.format(format, selected_index(s))
	end

	-- ---------------------------------------------------------------------
	-- Init
	-- ---------------------------------------------------------------------

	refresh()

	-- ---------------------------------------------------------------------
	-- Signals
	-- ---------------------------------------------------------------------

	awful.tag.attached_connect_signal(s, "property::selected", refresh)
	awful.tag.attached_connect_signal(s, "property::name", refresh)
	awful.tag.attached_connect_signal(s, "tagged", refresh)
	awful.tag.attached_connect_signal(s, "untagged", refresh)
	awful.tag.attached_connect_signal(s, "property::activated", refresh)
	s:connect_signal("tag::history::update", refresh)

	-- ---------------------------------------------------------------------
	-- Mouse
	-- ---------------------------------------------------------------------

	indicator:buttons(gears.table.join(
		awful.button({}, 4, function()
			awful.tag.viewnext(s)
		end),
		awful.button({}, 5, function()
			awful.tag.viewprev(s)
		end)
	))

	return {
		indicator = indicator,
	}
end

return M
