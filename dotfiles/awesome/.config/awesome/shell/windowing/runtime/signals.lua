-- ~/.config/awesome/shell/windowing/runtime/signals.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	windowing = {},
	signals_ready = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function windowing()
	return runtime.windowing or {}
end

local function focus_api()
	return windowing().focus
end

local function container_api()
	return windowing().container
end

local function titlebars_api()
	return windowing().titlebars
end

local function attach_titlebar_fn()
	return windowing().attach_titlebar
end

local function restyle(c)
	local Container = container_api()

	if Container and type(Container.apply) == "function" then
		Container.apply(c)
	end
end

local function titlebars_enabled_for(c)
	local Titlebars = titlebars_api()

	if Titlebars and type(Titlebars.enabled_for) == "function" then
		return Titlebars.enabled_for(c, windowing())
	end

	return true
end

local function sync_titlebar(c)
	if not (c and c.valid) then
		return
	end

	local attach_titlebar = attach_titlebar_fn()
	local enabled = titlebars_enabled_for(c)

	if enabled then
		c.titlebars_enabled = true

		if type(attach_titlebar) == "function" then
			attach_titlebar(c)
		end

		return
	end

	c.titlebars_enabled = false
	awful.titlebar.hide(c)
end

local function register_signals()
	if runtime.signals_ready then
		return
	end

	runtime.signals_ready = true

	client.connect_signal("request::titlebars", function(c)
		local attach_titlebar = attach_titlebar_fn()

		if titlebars_enabled_for(c) and type(attach_titlebar) == "function" then
			attach_titlebar(c)
		end
	end)

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			sync_titlebar(c)
		end)
	end)

	for _, prop in ipairs({
		"floating",
		"fullscreen",
		"maximized",
		"maximized_vertical",
		"maximized_horizontal",
	}) do
		client.connect_signal("property::" .. prop, function(c)
			sync_titlebar(c)
		end)
	end

	local Focus = focus_api()

	if Focus and Focus.on_mouse_enter then
		client.connect_signal("mouse::enter", function(c)
			Focus:on_mouse_enter(c)
		end)
	end

	if Focus and Focus.on_focus then
		client.connect_signal("focus", function(c)
			Focus:on_focus(c)
		end)
	end

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(restyle, c)
	end)

	client.connect_signal("focus", restyle)
	client.connect_signal("unfocus", restyle)

	for _, prop in ipairs({
		"maximized",
		"maximized_vertical",
		"maximized_horizontal",
		"fullscreen",
		"minimized",
	}) do
		client.connect_signal("property::" .. prop, restyle)
	end

	screen.connect_signal("arrange", function(s)
		for _, c in ipairs(s.clients) do
			restyle(c)
		end
	end)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.windowing = args.windowing or args or {}
	return M
end

function M.apply(args)
	args = args or {}

	if args.windowing then
		runtime.windowing = args.windowing
	else
		local w = runtime.windowing
		for k, v in pairs(args) do
			w[k] = v
		end
	end

	register_signals()
	return M
end

return M
