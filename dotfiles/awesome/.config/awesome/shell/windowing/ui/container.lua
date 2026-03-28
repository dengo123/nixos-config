-- ~/.config/awesome/shell/windowing/ui/container.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local C = {}

local runtime = {
	shape_fn = nil,
	rounded_corners = true,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function titlebar_buttons_mod(opts)
	return opts and opts.titlebar_buttons or nil
end

local function client_mouse_mod(opts)
	local input = (opts and opts.input) or {}
	local client = input.client or {}
	return client.mouse or {}
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

	runtime.shape_fn = o.shape_fn or nil
	runtime.rounded_corners = (o.rounded_corners ~= false)

	return C
end

function C.apply(c, _opts)
	if not (c and c.valid) then
		return
	end

	local maximized = c.fullscreen or c.maximized or c.maximized_vertical or c.maximized_horizontal

	local border_width = tonumber(beautiful.border_width)
	local border_normal = beautiful.border_normal
	local border_focus = beautiful.border_focus

	c.border_width = maximized and 0 or border_width
	c.border_color = (c == client.focus) and border_focus or border_normal

	local shape_fn = runtime.shape_fn

	if not shape_fn and runtime.rounded_corners ~= false then
		local radius = tonumber(beautiful.border_radius)

		if radius and radius > 0 then
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

	opts = opts or {}

	local Buttons = titlebar_buttons_mod(opts)
	assert(Buttons and type(Buttons.build) == "function", "windowing.container: titlebar_buttons.build fehlt")

	local ClientMouse = client_mouse_mod(opts)

	local pos = beautiful.titlebar_position
	local size = tonumber(beautiful.titlebar_height)

	local buttons = ClientMouse.titlebar_buttons and ClientMouse.titlebar_buttons(c, activate_client) or nil

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
