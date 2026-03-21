-- ~/.config/awesome/ui/wallpaper/apply.lua
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function fallback_color()
	return tostring(beautiful.wallpaper_bg or beautiful.bg_normal or "#000000")
end

local function fill_color_data(spec, Colors)
	local fit = spec.fit or {}

	if not Colors or type(Colors.resolve_sync) ~= "function" then
		return {
			style = tostring(fit.style or "solid"),
			solid = fallback_color(),
			top = fallback_color(),
			bottom = fallback_color(),
		}
	end

	return Colors.resolve_sync(spec.source, fit.style)
end

local function gradient_fill(data, s)
	local g = (s and s.geometry) or {}

	return {
		type = "linear",
		from = { 0, 0 },
		to = { 0, tonumber(g.height) or 1080 },
		stops = {
			{ 0, data.top or fallback_color() },
			{ 1, data.bottom or fallback_color() },
		},
	}
end

local function solid_or_gradient_fill(spec, Colors, s)
	local fit = spec.fit or {}
	local fill = fill_color_data(spec, Colors)

	if fit.style == "gradient" then
		return gradient_fill(fill, s)
	end

	return fill.solid or fallback_color()
end

local function desktop_widget(spec, s, Scope)
	local vg = Scope.virtual_geometry()
	local off = Scope.screen_offset(s, vg)
	local sg = s.geometry

	local canvas = wibox.widget({
		{
			image = spec.source,
			resize = true,
			horizontal_fit_policy = "cover",
			vertical_fit_policy = "cover",
			widget = wibox.widget.imagebox,
		},
		widget = wibox.container.constraint,
		strategy = "exact",
		width = vg.width,
		height = vg.height,
	})

	return wibox.widget({
		{
			canvas,
			left = -off.x,
			right = 0,
			top = -off.y,
			bottom = 0,
			widget = wibox.container.margin,
		},
		widget = wibox.container.constraint,
		strategy = "exact",
		width = sg.width,
		height = sg.height,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply_screen(s, spec, Colors)
	local fit = spec.fit or {}

	if fit.enabled == true then
		local fill = solid_or_gradient_fill(spec, Colors, s)

		if gears.wallpaper.fit then
			gears.wallpaper.fit(spec.source, s, fill)
			return
		end

		if gears.wallpaper.centered then
			gears.wallpaper.centered(spec.source, s, fill, true)
			return
		end
	end

	gears.wallpaper.maximized(spec.source, s, false)
end

function M.apply_desktop(s, spec, Scope, _Colors)
	gears.wallpaper.set(desktop_widget(spec, s, Scope), s)
end

return M
