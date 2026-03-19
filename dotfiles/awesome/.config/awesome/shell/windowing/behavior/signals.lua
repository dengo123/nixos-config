-- ~/.config/awesome/shell/windowing/policies/signals.lua
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

	local focus = o.focus
	local container = o.container
	local attach_titlebar = o.attach_titlebar

	-- ---------------------------------------------------------------------
	-- Helpers
	-- ---------------------------------------------------------------------

	local function restyle(c)
		if container and container.apply then
			container.apply(c)
		end
	end

	-- ---------------------------------------------------------------------
	-- Titlebars
	-- ---------------------------------------------------------------------

	client.connect_signal("request::titlebars", function(c)
		if type(attach_titlebar) == "function" then
			attach_titlebar(c)
		end
	end)

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
