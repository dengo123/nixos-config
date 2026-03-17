-- ~/.config/awesome/shell/windowing/titlebar.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local B = {}

-- =========================================================================
-- Theme Assets
-- =========================================================================

local function ensure_titlebar_assets()
	if beautiful.titlebar_close_button_normal then
		return
	end

	local path = gears.filesystem.get_themes_dir() .. "default/theme.lua"
	local ok, def = pcall(dofile, path)
	if not ok or type(def) ~= "table" then
		return
	end

	for _, key in ipairs({
		"titlebar_close_button_normal",
		"titlebar_close_button_focus",
		"titlebar_minimize_button_normal",
		"titlebar_minimize_button_focus",
		"titlebar_floating_button_normal_active",
		"titlebar_floating_button_focus_active",
		"titlebar_floating_button_normal_inactive",
		"titlebar_floating_button_focus_inactive",
	}) do
		if beautiful[key] == nil and def[key] ~= nil then
			beautiful[key] = def[key]
		end
	end
end

-- =========================================================================
-- Helpers
-- =========================================================================

local function suppress_center(sec)
	awesome.emit_signal("ui::suppress_center", sec or 0.2)
end

local function has_tag(c, t)
	if not (c and c.valid and t) then
		return false
	end

	for _, ct in ipairs(c:tags() or {}) do
		if ct == t then
			return true
		end
	end

	return false
end

local function pick_next_focus(c)
	local s = c and c.screen
	local t = c and c.first_tag

	if not (c and c.valid and s) then
		return nil
	end

	for cl in
		awful.client.iterate(function(x)
			return x ~= c and x.valid and not x.minimized and x:isvisible() and x.screen == s and t and has_tag(x, t)
		end, nil, s)
	do
		return cl
	end

	for cl in
		awful.client.iterate(function(x)
			return x ~= c and x.valid and not x.minimized and x:isvisible() and x.screen == s
		end, nil, s)
	do
		return cl
	end

	return nil
end

local function activate_client(c, context, raise)
	if not (c and c.valid) then
		return
	end

	c:emit_signal("request::activate", context or "titlebar", {
		raise = (raise ~= false),
	})
end

local function recolor_imagebox(ib, img, color)
	if img then
		ib.image = gears.color.recolor_image(img, color)
	end
end

-- =========================================================================
-- Image Button
-- =========================================================================

local function make_img_button(opts)
	local bar_h = assert(tonumber(opts.size), "titlebar: button size fehlt/ungültig")
	local inner = math.floor(bar_h * 0.90)
	local pad = math.floor((bar_h - inner) / 2)
	local active = opts.img_active or opts.img

	local ib = wibox.widget({
		image = active and gears.color.recolor_image(active, opts.color) or nil,
		resize = true,
		forced_width = inner,
		forced_height = inner,
		widget = wibox.widget.imagebox,
	})

	local bg = wibox.widget({
		{
			ib,
			left = pad,
			right = pad,
			top = pad,
			bottom = pad,
			widget = wibox.container.margin,
		},
		widget = wibox.container.background,
	})

	bg:connect_signal("mouse::enter", function()
		recolor_imagebox(ib, ib._current_img or active, opts.color_hover or opts.color)
	end)

	bg:connect_signal("mouse::leave", function()
		recolor_imagebox(ib, ib._current_img or active, opts.color)
	end)

	bg:buttons(gears.table.join(awful.button({}, 1, function()
		if type(opts.on_click) == "function" then
			opts.on_click(ib, recolor_imagebox)
		end
	end)))

	if type(opts.on_update) == "function" then
		opts.on_update(ib, recolor_imagebox)
	end

	return bg, ib
end

-- =========================================================================
-- Buttons
-- =========================================================================

function B.build_buttons(c, style)
	ensure_titlebar_assets()

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	style = style or {}

	local size = assert(tonumber(style.size), "titlebar: style.size fehlt/ungültig")
	local spacing = assert(tonumber(style.spacing), "titlebar: style.spacing fehlt/ungültig")
	local fg = assert(style.fg, "titlebar: style.fg fehlt")
	local fg_hover = assert(style.fg_hover, "titlebar: style.fg_hover fehlt")
	local close_fg = assert(style.close, "titlebar: style.close fehlt")
	local close_hover = assert(style.close_hover, "titlebar: style.close_hover fehlt")

	local img_close = beautiful.titlebar_close_button_normal
	local img_minimize = beautiful.titlebar_minimize_button_normal
	local img_float_on = beautiful.titlebar_floating_button_normal_active
		or beautiful.titlebar_floating_button_focus_active
	local img_float_off = beautiful.titlebar_floating_button_normal_inactive
		or beautiful.titlebar_floating_button_focus_inactive
		or beautiful.titlebar_floating_button_normal

	-- ---------------------------------------------------------------------
	-- Minimize
	-- ---------------------------------------------------------------------

	local btn_min = make_img_button({
		img_active = img_minimize,
		color = fg,
		color_hover = fg_hover,
		size = size,
		on_click = function()
			suppress_center(0.2)

			local nextc = pick_next_focus(c)
			if not nextc or not nextc.valid then
				nextc = awful.client.focus.history.get(c.screen, 1)
				if nextc == c then
					nextc = nil
				end
			end

			gears.timer.delayed_call(function()
				if not (c and c.valid) then
					return
				end

				c.minimized = true

				if nextc and nextc.valid and not nextc.minimized then
					activate_client(nextc, "titlebar_minimize_fallback", true)
				end
			end)
		end,
	})

	-- ---------------------------------------------------------------------
	-- Floating
	-- ---------------------------------------------------------------------

	local function update_float_icon(ib, recolor)
		local img = c.floating and (img_float_on or img_float_off) or img_float_off
		ib._current_img = img
		recolor(ib, img, fg)
	end

	local btn_float, ib_float = make_img_button({
		img_active = img_float_on or img_float_off,
		img_inactive = img_float_off,
		color = fg,
		color_hover = fg_hover,
		size = size,
		on_click = function(ib, recolor)
			activate_client(c, "titlebar_float_toggle", true)
			awful.client.floating.toggle(c)
			update_float_icon(ib, recolor)
		end,
		on_update = update_float_icon,
	})

	c:connect_signal("property::floating", function()
		update_float_icon(ib_float, recolor_imagebox)
	end)

	-- ---------------------------------------------------------------------
	-- Close
	-- ---------------------------------------------------------------------

	local btn_close = make_img_button({
		img_active = img_close,
		color = close_fg,
		color_hover = close_hover,
		size = size,
		on_click = function()
			activate_client(c, "titlebar_close", true)
			c:kill()
		end,
	})

	-- ---------------------------------------------------------------------
	-- Layout
	-- ---------------------------------------------------------------------

	return {
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
		btn_min,
		btn_float,
		btn_close,
	}
end

-- =========================================================================
-- Public API
-- =========================================================================

function B.attach_titlebar(c, style)
	if not (c and c.valid) then
		return
	end

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local pos = assert(beautiful.titlebar_position, "titlebar: beautiful.titlebar_position fehlt")
	local size = assert(tonumber(beautiful.titlebar_height), "titlebar: beautiful.titlebar_height fehlt/ungültig")

	-- ---------------------------------------------------------------------
	-- Controls
	-- ---------------------------------------------------------------------

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

	-- ---------------------------------------------------------------------
	-- Build
	-- ---------------------------------------------------------------------

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
			B.build_buttons(c, style),
			layout = wibox.layout.align.horizontal,
		})
end

return B
