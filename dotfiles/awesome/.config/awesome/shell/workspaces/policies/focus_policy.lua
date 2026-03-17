-- ~/.config/awesome/shell/workspaces/policies/focus_policy.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

local LAST_FOCUS = setmetatable({}, { __mode = "k" })

-- =========================================================================
-- Helpers
-- =========================================================================

local function ensure_record_for_tag(t)
	local record = LAST_FOCUS[t]

	if not record then
		record = {
			last_any = nil,
			by_screen = setmetatable({}, { __mode = "k" }),
		}

		LAST_FOCUS[t] = record
	end

	return record
end

local function set_last_focus(tag, screen, c)
	if not (tag and screen and c and c.valid) then
		return
	end

	local record = ensure_record_for_tag(tag)
	record.last_any = c
	record.by_screen[screen] = c
end

local function get_last_focus_for(tag, screen)
	local record = LAST_FOCUS[tag]
	if not record then
		return nil
	end

	local c = record.by_screen[screen]
	if c and c.valid then
		return c
	end

	c = record.last_any
	if c and c.valid then
		return c
	end

	return nil
end

local function with_mouse_lock(fn)
	local pos = mouse.coords()

	awesome.emit_signal("ui::suppress_center", 0.25)

	fn()

	gears.timer.delayed_call(function()
		if pos and pos.x and pos.y then
			mouse.coords(pos, true)
		end
	end)
end

local function client_on_tag_and_screen(c, tag, screen)
	return c and c.valid and c.screen == screen and c.first_tag == tag
end

local function focus_last_of_tag_on_screen_or_fallback(tag, screen)
	if not (tag and screen) then
		return
	end

	if awful.screen.focused() ~= screen then
		return
	end

	gears.timer.delayed_call(function()
		if awful.screen.focused() ~= screen then
			return
		end

		-- -----------------------------------------------------------------
		-- Remembered
		-- -----------------------------------------------------------------

		local c = get_last_focus_for(tag, screen)
		if not client_on_tag_and_screen(c, tag, screen) then
			c = nil
		end

		-- -----------------------------------------------------------------
		-- Tiled Fallback
		-- -----------------------------------------------------------------

		if not c then
			local tiled = awful.client.tiled(screen)

			if tiled then
				for _, x in ipairs(tiled) do
					if client_on_tag_and_screen(x, tag, screen) then
						c = x
						break
					end
				end
			end
		end

		-- -----------------------------------------------------------------
		-- Any Client Fallback
		-- -----------------------------------------------------------------

		if not c then
			local list = tag:clients()

			if list then
				for _, x in ipairs(list) do
					if client_on_tag_and_screen(x, tag, screen) then
						c = x
						break
					end
				end
			end
		end

		-- -----------------------------------------------------------------
		-- Activate
		-- -----------------------------------------------------------------

		if c and c.valid then
			if c.minimized then
				c.minimized = false
			end

			c:emit_signal("request::activate", "tag_switch_last_focus_per_screen", { raise = true })

			gears.timer.delayed_call(function()
				if awful.screen.focused() ~= screen then
					return
				end

				if client.focus and client.focus.valid and client.focus.screen ~= screen then
					c:emit_signal("request::activate", "tag_switch_enforce_screen", { raise = true })
				end
			end)
		end
	end)
end

-- =========================================================================
-- Learning
-- =========================================================================

client.connect_signal("focus", function(c)
	if not (c and c.valid) then
		return
	end

	local tag = c.first_tag
	local screen = c.screen

	if tag and screen then
		set_last_focus(tag, screen, c)
	end
end)

client.connect_signal("tagged", function(c, tag)
	if not (c and c.valid and tag) then
		return
	end

	local screen = c.screen

	if client.focus == c and screen then
		set_last_focus(tag, screen, c)
	end
end)

client.connect_signal("untagged", function(c, tag)
	local record = LAST_FOCUS[tag]
	if not (record and c) then
		return
	end

	if record.last_any == c then
		record.last_any = nil
	end

	for screen, client_on_screen in pairs(record.by_screen) do
		if client_on_screen == c then
			record.by_screen[screen] = nil
		end
	end
end)

client.connect_signal("unmanage", function(c)
	if not c then
		return
	end

	for _, record in pairs(LAST_FOCUS) do
		if record.last_any == c then
			record.last_any = nil
		end

		for screen, client_on_screen in pairs(record.by_screen) do
			if client_on_screen == c then
				record.by_screen[screen] = nil
			end
		end
	end
end)

-- =========================================================================
-- Public API
-- =========================================================================

function M.focus_master_current(screen)
	screen = screen or awful.screen.focused()

	local tag = screen and screen.selected_tag
	if not tag then
		return
	end

	focus_last_of_tag_on_screen_or_fallback(tag, screen)
end

function M.attach_policy_signals(apply_layout_policy_fn)
	tag.connect_signal("property::selected", function(tag)
		if not (tag and tag.selected and tag.screen) then
			return
		end

		if awesome._ws_sync_busy and tag.screen ~= awful.screen.focused() then
			return
		end

		if tag.screen ~= awful.screen.focused() then
			return
		end

		with_mouse_lock(function()
			if type(apply_layout_policy_fn) == "function" then
				apply_layout_policy_fn(tag.screen)
			end

			focus_last_of_tag_on_screen_or_fallback(tag, tag.screen)
		end)
	end)
end

return M
