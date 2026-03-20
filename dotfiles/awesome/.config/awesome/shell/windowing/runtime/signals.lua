-- ~/.config/awesome/shell/windowing/runtime/signals.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply(o)
	o = o or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local api = o.api or {}
	local focus = o.focus
	local container = o.container
	local attach_titlebar = o.attach_titlebar

	local titlebars = api.titlebars

	-- ---------------------------------------------------------------------
	-- Helpers
	-- ---------------------------------------------------------------------

	local function restyle(c)
		if container and container.apply then
			container.apply(c, {
				api = api,
			})
		end
	end

	local function titlebars_enabled_for(c)
		if titlebars and type(titlebars.enabled_for) == "function" then
			return titlebars.enabled_for(c, api)
		end

		return true
	end

	local function sync_titlebar(c)
		if not (c and c.valid) then
			return
		end

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

	-- ---------------------------------------------------------------------
	-- Titlebars
	-- ---------------------------------------------------------------------

	client.connect_signal("request::titlebars", function(c)
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

	-- ---------------------------------------------------------------------
	-- Focus
	-- ---------------------------------------------------------------------

	if focus and focus.on_mouse_enter then
		client.connect_signal("mouse::enter", function(c)
			focus:on_mouse_enter(c)
		end)
	end

	if focus and focus.on_focus then
		client.connect_signal("focus", function(c)
			focus:on_focus(c)
		end)
	end

	-- ---------------------------------------------------------------------
	-- Container
	-- ---------------------------------------------------------------------

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

return M
