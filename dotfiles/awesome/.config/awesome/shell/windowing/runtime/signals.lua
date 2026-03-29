-- ~/.config/awesome/shell/windowing/runtime/signals.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local runtime = {
	windowing = {},
	signals_ready = false,
	default_mousebindings_ready = false,
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

local function modset(modkey)
	if type(modkey) == "string" and modkey ~= "" then
		return { modkey }
	end

	return {}
end

local function activate_client(c, context, raise)
	if not (c and c.valid) then
		return
	end

	c:emit_signal("request::activate", context or "mouse_click", {
		raise = (raise ~= false),
	})
end

local function default_mousebindings(modkey)
	local mods = modset(modkey)

	return {
		awful.button({}, 1, function(c)
			activate_client(c, "mouse_click", true)
		end),

		awful.button(mods, 1, function(c)
			activate_client(c, "mouse_click", true)
			awful.mouse.client.move(c)
		end),

		awful.button(mods, 3, function(c)
			activate_client(c, "mouse_click", true)
			awful.mouse.client.resize(c)
		end),
	}
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

local function register_default_mousebindings()
	if runtime.default_mousebindings_ready then
		return true
	end

	awful.mouse.append_client_mousebindings(default_mousebindings(current_modkey()))
	runtime.default_mousebindings_ready = true
	return true
end

local function register_signals()
	if runtime.signals_ready then
		return
	end

	runtime.signals_ready = true

	register_default_mousebindings()

	client.connect_signal("request::titlebars", function(c)
		local attach_titlebar = attach_titlebar_fn()

		if titlebars_enabled_for(c) and type(attach_titlebar) == "function" then
			attach_titlebar(c)
		end
	end)

	client.connect_signal("manage", function(c)
		gears.timer.delayed_call(function()
			register_default_mousebindings()
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

	register_default_mousebindings()
	register_signals()
	return M
end

return M
