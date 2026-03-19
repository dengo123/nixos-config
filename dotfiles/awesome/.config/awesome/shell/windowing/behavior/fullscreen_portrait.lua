-- ~/.config/awesome/shell/windowing/policies/fullscreen_portrait.lua
local gears = require("gears")

local M = {}

local ENABLED = true
local signals_ready = false

-- =========================================================================
-- Helpers
-- =========================================================================

local function is_portrait_screen(s)
	if not s then
		return false
	end

	local wa = s.workarea or s.geometry
	if not wa then
		return false
	end

	return wa.height > wa.width
end

local function is_tiled_client(c)
	if not (c and c.valid) then
		return false
	end

	if c.floating then
		return false
	end

	if c.minimized then
		return false
	end

	if c.type ~= "normal" then
		return false
	end

	return true
end

local function should_force(c)
	if not ENABLED then
		return false
	end

	if not (c and c.valid and c.screen) then
		return false
	end

	if not is_portrait_screen(c.screen) then
		return false
	end

	if not is_tiled_client(c) then
		return false
	end

	if c.fullscreen then
		return false
	end

	return true
end

local function should_release(c)
	if not (c and c.valid and c.screen) then
		return false
	end

	if c._portrait_forced_fullscreen ~= true then
		return false
	end

	if c.floating then
		return true
	end

	return not is_portrait_screen(c.screen)
end

local function apply_client(c)
	if not (c and c.valid) then
		return
	end

	if c._portrait_fullscreen_busy then
		return
	end

	c._portrait_fullscreen_busy = true

	if should_force(c) then
		c._portrait_forced_fullscreen = true
		c.fullscreen = true
	elseif should_release(c) then
		c._portrait_forced_fullscreen = nil
		c.fullscreen = false
	end

	c._portrait_fullscreen_busy = nil
end

local function apply_screen(s)
	if not s then
		return
	end

	for _, c in ipairs(s.clients) do
		apply_client(c)
	end
end

local function delayed_apply_client(c, sec)
	gears.timer.start_new(sec or 0.20, function()
		apply_client(c)
		return false
	end)
end

local function delayed_apply_screen(s, sec)
	gears.timer.start_new(sec or 0.20, function()
		apply_screen(s)
		return false
	end)
end

local function setup_signals()
	if signals_ready then
		return
	end

	client.connect_signal("manage", function(c)
		delayed_apply_client(c, 0.20)
		if c and c.screen then
			delayed_apply_screen(c.screen, 0.25)
		end
	end)

	client.connect_signal("property::floating", function(c)
		delayed_apply_client(c, 0.05)
		if c and c.screen then
			delayed_apply_screen(c.screen, 0.05)
		end
	end)

	client.connect_signal("property::screen", function(c)
		delayed_apply_client(c, 0.05)
		if c and c.screen then
			delayed_apply_screen(c.screen, 0.05)
		end
	end)

	client.connect_signal("unmanage", function(c)
		local s = c and c.screen or nil

		if c then
			c._portrait_forced_fullscreen = nil
			c._portrait_fullscreen_busy = nil
		end

		if s then
			delayed_apply_screen(s, 0.05)
		end
	end)

	screen.connect_signal("property::geometry", function(s)
		delayed_apply_screen(s, 0.05)
	end)

	tag.connect_signal("property::selected", function(t)
		if t and t.screen then
			delayed_apply_screen(t.screen, 0.05)
		end
	end)

	signals_ready = true
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	if opts.enabled ~= nil then
		ENABLED = (opts.enabled == true)
	end

	-- ---------------------------------------------------------------------
	-- Setup
	-- ---------------------------------------------------------------------

	setup_signals()

	if not ENABLED then
		for s in screen do
			for _, c in ipairs(s.clients) do
				if c._portrait_forced_fullscreen == true then
					c._portrait_forced_fullscreen = nil
					c.fullscreen = false
					c:emit_signal("request::titlebars")
				end
			end
		end
		return
	end

	-- ---------------------------------------------------------------------
	-- Initial
	-- ---------------------------------------------------------------------

	for s in screen do
		delayed_apply_screen(s, 0.10)
	end
end

return M
