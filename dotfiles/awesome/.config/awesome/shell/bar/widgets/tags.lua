-- ~/.config/awesome/shell/bar/widgets/tags.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function as_table(value, fallback)
	if type(value) == "table" then
		return value
	end
	return fallback or {}
end

local function as_number(value, fallback)
	local n = tonumber(value)
	if n ~= nil then
		return n
	end
	return fallback
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
	local tags = (s and s.tags) or {}
	return #tags
end

local function selected_index(s)
	local focused = awful.screen.focused()
	local t = (s and s.selected_tag) or (focused and focused.selected_tag)
	return (t and t.index) or 1
end

local function tags_mode(opts)
	local cfg = (opts and opts.cfg) or {}
	local tags_cfg = cfg.tags or {}
	return string.lower(tostring(tags_cfg.mode or "fixed"))
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(s, opts)
	opts = opts or {}

	local mode = tags_mode(opts)

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local theme = as_table(beautiful.tags_indicator, {})

	local pad_h = as_number(opts.pad_h, as_number(theme.pad_h, 8)) or 8
	local pad_v = as_number(opts.pad_v, as_number(theme.pad_v, 2)) or 2
	local collapsed_pad_h = as_number(theme.collapsed_pad_h, pad_h) or pad_h

	local font = first_string(opts.font, theme.font, beautiful.font, "Sans 10")
	local format = first_string(opts.format, theme.fmt, "%d")

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
	-- State
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
		if mode == "dynamic" and tag_count(s) <= 1 then
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
