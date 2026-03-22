-- ~/.config/awesome/ui/wallpaper/apply.lua
local awful_wallpaper = require("awful.wallpaper")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local lgi = require("lgi")
local cairo = lgi.cairo

local M = {}

local wallpapers = {}
local cover_cache = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function fallback_color()
	return tostring(beautiful.wallpaper_bg or beautiful.bg_normal or "#000000")
end

local function fill_color_data(spec, Colors)
	local display = spec.display or {}

	if not Colors or type(Colors.resolve_sync) ~= "function" then
		return {
			style = tostring(display.fill or "solid"),
			solid = fallback_color(),
			top = fallback_color(),
			bottom = fallback_color(),
		}
	end

	return Colors.resolve_sync(spec.source, display.fill)
end

local function gradient_fill(data, height)
	return {
		type = "linear",
		from = { 0, 0 },
		to = { 0, tonumber(height) or 1080 },
		stops = {
			{ 0, data.top or fallback_color() },
			{ 1, data.bottom or fallback_color() },
		},
	}
end

local function fit_fill_background(spec, Colors, height)
	local display = spec.display or {}
	local fill = fill_color_data(spec, Colors)

	if tostring(display.fill or "solid"):lower() == "gradient" then
		return gradient_fill(fill, height)
	end

	return fill.solid or fallback_color()
end

local function wallpaper_key(s)
	return tostring((s and s.index) or 0)
end

local function wallpaper_for_screen(s)
	local key = wallpaper_key(s)
	local wp = wallpapers[key]

	if wp then
		return wp
	end

	wp = awful_wallpaper({
		screen = s,
		bg = fallback_color(),
		widget = nil,
	})

	wallpapers[key] = wp
	return wp
end

local function set_wallpaper(s, bg, widget)
	local wp = wallpaper_for_screen(s)
	wp.screen = s
	wp.bg = bg or fallback_color()
	wp.widget = widget
end

local function screen_geometry(s)
	local g = (s and s.geometry) or {}

	return {
		x = tonumber(g.x) or 0,
		y = tonumber(g.y) or 0,
		width = tonumber(g.width) or 0,
		height = tonumber(g.height) or 0,
	}
end

local function geometry_for_span_group(Scope, s, spec)
	if Scope and type(Scope.geometry_for_span_group) == "function" then
		local group_geom, offset = Scope.geometry_for_span_group(s, spec)
		if group_geom and offset then
			return group_geom, offset
		end
	end

	if Scope and type(Scope.virtual_geometry) == "function" and type(Scope.screen_offset) == "function" then
		local vg = Scope.virtual_geometry()
		local off = Scope.screen_offset(s, vg)
		return vg, off
	end

	local sg = screen_geometry(s)
	return sg, { x = 0, y = 0 }
end

local function surface_key(source, width, height)
	return table.concat({
		tostring(source or ""),
		tostring(width or 0),
		tostring(height or 0),
	}, "::")
end

local function render_cover_surface(source, width, height)
	width = math.max(1, math.floor(tonumber(width) or 1))
	height = math.max(1, math.floor(tonumber(height) or 1))

	local key = surface_key(source, width, height)
	if cover_cache[key] then
		return cover_cache[key]
	end

	local src = gears.surface.load_uncached(source)
	if not src then
		return nil
	end

	local sw = tonumber(src.get_width and src:get_width()) or 0
	local sh = tonumber(src.get_height and src:get_height()) or 0

	if sw <= 0 or sh <= 0 then
		return nil
	end

	local scale = math.max(width / sw, height / sh)
	local scaled_w = sw * scale
	local scaled_h = sh * scale
	local dx = (width - scaled_w) / 2
	local dy = (height - scaled_h) / 2

	local out = cairo.ImageSurface.create(cairo.Format.ARGB32, width, height)
	local cr = cairo.Context(out)

	cr:save()
	cr:translate(dx, dy)
	cr:scale(scale, scale)
	cr:set_source_surface(src, 0, 0)
	cr:paint()
	cr:restore()

	cover_cache[key] = out
	return out
end

local function invalidate_cover_cache()
	cover_cache = {}
end

local function maximized_widget_for_geometry(source, width, height)
	local rendered = render_cover_surface(source, width, height)

	return wibox.widget({
		image = rendered or source,
		resize = false,
		widget = wibox.widget.imagebox,
	})
end

local function fit_fill_widget_for_geometry(source, width, height)
	return wibox.widget({
		{
			{
				image = source,
				resize = true,
				horizontal_fit_policy = "fit",
				vertical_fit_policy = "fit",
				widget = wibox.widget.imagebox,
			},
			valign = "center",
			halign = "center",
			widget = wibox.container.place,
		},
		widget = wibox.container.constraint,
		strategy = "exact",
		width = width,
		height = height,
	})
end

local function center_widget_for_geometry(source, width, height)
	return wibox.widget({
		{
			{
				image = source,
				resize = false,
				widget = wibox.widget.imagebox,
			},
			valign = "center",
			halign = "center",
			widget = wibox.container.place,
		},
		widget = wibox.container.constraint,
		strategy = "exact",
		width = width,
		height = height,
	})
end

local function stretch_widget_for_geometry(source, width, height)
	return wibox.widget({
		{
			image = source,
			resize = true,
			horizontal_fit_policy = "fit",
			vertical_fit_policy = "fit",
			widget = wibox.widget.imagebox,
		},
		widget = wibox.container.constraint,
		strategy = "exact",
		width = width,
		height = height,
	})
end

local function widget_for_mode(source, mode, width, height)
	mode = tostring(mode or "maximized"):lower()

	if mode == "fit_fill" then
		return fit_fill_widget_for_geometry(source, width, height)
	end

	if mode == "center" then
		return center_widget_for_geometry(source, width, height)
	end

	if mode == "stretch" then
		return stretch_widget_for_geometry(source, width, height)
	end

	return maximized_widget_for_geometry(source, width, height)
end

local function desktop_widget(spec, s, Scope)
	local group_geom, off = geometry_for_span_group(Scope, s, spec)
	local sg = screen_geometry(s)

	local canvas = widget_for_mode(
		spec.source,
		spec.display and spec.display.mode or "maximized",
		group_geom.width,
		group_geom.height
	)

	return wibox.widget({
		{
			canvas,
			left = -(tonumber(off.x) or 0),
			top = -(tonumber(off.y) or 0),
			widget = wibox.container.margin,
		},
		widget = wibox.container.constraint,
		strategy = "exact",
		width = sg.width,
		height = sg.height,
	})
end

local function screen_widget(spec, s)
	local sg = screen_geometry(s)
	return widget_for_mode(spec.source, spec.display and spec.display.mode or "maximized", sg.width, sg.height)
end

local function background_for_screen(spec, Colors, s)
	local mode = tostring((spec.display and spec.display.mode) or "maximized"):lower()
	local sg = screen_geometry(s)

	if mode == "fit_fill" then
		return fit_fill_background(spec, Colors, sg.height)
	end

	return fallback_color()
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply_screen(s, spec, Colors)
	if not (s and spec and spec.source) then
		return
	end

	set_wallpaper(s, background_for_screen(spec, Colors, s), screen_widget(spec, s))
end

function M.apply_desktop(s, spec, Scope, Colors)
	if not (s and spec and spec.source and Scope) then
		return
	end

	local group_geom = geometry_for_span_group(Scope, s, spec)
	local bg = fallback_color()

	if tostring((spec.display and spec.display.mode) or "maximized"):lower() == "fit_fill" then
		bg = fit_fill_background(spec, Colors, group_geom and group_geom.height or nil)
	end

	set_wallpaper(s, bg, desktop_widget(spec, s, Scope))
end

function M.invalidate_cache()
	invalidate_cover_cache()
end

return M
