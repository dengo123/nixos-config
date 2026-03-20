-- ~/.config/awesome/shell/windowing/ui/container.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local C = {}

local runtime_api = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function current_api(opts)
	return (opts and opts.api) or runtime_api
end

local function buttons_api(opts)
	local api = current_api(opts)
	return api and api.titlebar_buttons or nil
end

local function activate_client(c, context, raise)
	if not (c and c.valid) then
		return
	end

	c:emit_signal("request::activate", context or "titlebar", {
		raise = (raise ~= false),
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function C.init(o)
	o = o or {}

	runtime_api = o.api or {}
	C.shape_fn = o.shape_fn or nil
	C.rounded_corners = (o.rounded_corners ~= false)
end

function C.apply(c, _opts)
	if not (c and c.valid) then
		return
	end

	local maximized = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal

	local border_width =
		assert(tonumber(beautiful.border_width), "windowing.container: beautiful.border_width fehlt/ungültig")
	local border_normal = assert(beautiful.border_normal, "windowing.container: beautiful.border_normal fehlt")
	local border_focus = assert(beautiful.border_focus, "windowing.container: beautiful.border_focus fehlt")

	c.border_width = maximized and 0 or border_width
	c.border_color = (c == client.focus) and border_focus or border_normal

	local shape_fn = C.shape_fn

	if not shape_fn and C.rounded_corners ~= false then
		local radius =
			assert(tonumber(beautiful.border_radius), "windowing.container: beautiful.border_radius fehlt/ungültig")

		if radius > 0 then
			shape_fn = function(cr, w, h)
				if (w or 0) >= 2 and (h or 0) >= 2 then
					gears.shape.rounded_rect(cr, w, h, radius)
				end
			end
		end
	end

	c.shape = (not maximized and shape_fn) or nil
end

function C.attach_titlebar(c, style, actions, cfg, opts)
	if not (c and c.valid) then
		return
	end

	local Buttons = buttons_api(opts)
	assert(Buttons and type(Buttons.build) == "function", "windowing.container: api.titlebar_buttons.build fehlt")

	local pos = assert(beautiful.titlebar_position, "container: beautiful.titlebar_position fehlt")
	local size = assert(tonumber(beautiful.titlebar_height), "container: beautiful.titlebar_height fehlt/ungültig")

	local buttons = gears.table.join(
		awful.button({}, 1, function()
			activate_client(c, "titlebar_drag", true)
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			activate_client(c, "titlebar_resize", true)
			awful.mouse.client.resize(c)
		end)
	)

	local title = awful.titlebar.widget.titlewidget(c)
	if title.set_align then
		title:set_align("left")
	end

	awful
		.titlebar(c, {
			position = pos,
			size = size,
		})
		:setup({
			{
				awful.titlebar.widget.iconwidget(c),
				title,
				buttons = buttons,
				spacing = 6,
				layout = wibox.layout.fixed.horizontal,
			},
			{
				nil,
				nil,
				nil,
				buttons = buttons,
				layout = wibox.layout.align.horizontal,
			},
			Buttons.build(c, style, actions, cfg),
			layout = wibox.layout.align.horizontal,
		})
end

return C
