-- ~/.config/awesome/shell/windowing/runtime/signals.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	windowing = {},
	signals_ready = false,
	default_mousebindings_ready = false,
	default_mousebindings_retry_timer = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function windowing()
	return runtime.windowing or {}
end

local function focus_mod()
	return windowing().focus
end

local function container_mod()
	return windowing().container
end

local function titlebars_mod()
	return windowing().titlebars
end

local function attach_titlebar_fn()
	return windowing().attach_titlebar
end

local function input_public()
	local input = windowing().input or {}
	return input.public or {}
end

local function client_mouse_mod()
	local public = input_public()
	local client = public.client or {}
	return client.mouse or nil
end

local function current_modkey()
	return windowing().modkey
end

local function restyle(c)
	local Container = container_mod()

	if Container and type(Container.apply) == "function" then
		Container.apply(c)
	end
end

local function titlebars_enabled_for(c)
	local Titlebars = titlebars_mod()

	if Titlebars and type(Titlebars.enabled_for) == "function" then
		return Titlebars.enabled_for(c)
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

local function stop_default_mousebindings_retry()
	if runtime.default_mousebindings_retry_timer then
		runtime.default_mousebindings_retry_timer:stop()
		runtime.default_mousebindings_retry_timer = nil
	end
end

local function try_register_default_mousebindings()
	if runtime.default_mousebindings_ready then
		stop_default_mousebindings_retry()
		return true
	end

	local Mouse = client_mouse_mod()
	if not (Mouse and type(Mouse.default_mousebindings) == "function") then
		return false
	end

	awful.mouse.append_client_mousebindings(Mouse.default_mousebindings(current_modkey()))
	runtime.default_mousebindings_ready = true
	stop_default_mousebindings_retry()
	return true
end

local function ensure_default_mousebindings_registered()
	if try_register_default_mousebindings() then
		return
	end

	if runtime.default_mousebindings_retry_timer then
		return
	end

	runtime.default_mousebindings_retry_timer = gears.timer({
		timeout = 0.25,
		autostart = true,
		call_now = false,
		callback = function()
			try_register_default_mousebindings()
		end,
	})
end

local function register_signals()
	if runtime.signals_ready then
		return
	end

	runtime.signals_ready = true

	ensure_default_mousebindings_registered()

	client.connect_signal("request::titlebars", function(c)
		local attach_titlebar = attach_titlebar_fn()

		if titlebars_enabled_for(c) and type(attach_titlebar) == "function" then
			attach_titlebar(c)
		end
	end)

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			ensure_default_mousebindings_registered()
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

	local Focus = focus_mod()

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
	end

	ensure_default_mousebindings_registered()
	register_signals()
	return M
end

return M
