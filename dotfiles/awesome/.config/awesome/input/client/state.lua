-- ~/.config/awesome/input/client/state.lua
local awful = require("awful")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function focused_client()
	return client.focus
end

local function focused_screen()
	return awful.screen.focused()
end

local function toggle_native_fullscreen()
	local c = focused_client()
	if not c then
		return
	end

	c.fullscreen = not c.fullscreen
	c:raise()
end

local function restore_local(actions)
	if actions and type(actions.current_screen) == "function" then
		awesome.emit_signal("windowing::restore_request", actions.current_screen())
		return
	end

	awesome.emit_signal("windowing::restore_request", focused_screen())
end

local function toggle_minimized(actions)
	local c = focused_client()

	if c and not c.minimized then
		awesome.emit_signal("ui::suppress_center", 0.2)

		if actions and type(actions.minimize_focused) == "function" then
			actions.minimize_focused()
		else
			c.minimized = true
		end
		return
	end

	restore_local(actions)
end

local function toggle_scope_minimize(actions, minimize_fn)
	local ok = false
	if type(minimize_fn) == "function" then
		ok = (minimize_fn(actions) == true)
	end

	if ok then
		return
	end

	restore_local(actions)
end

local function toggle_visible_tag_on_screen(actions)
	toggle_scope_minimize(actions, function(a)
		if a and type(a.minimize_visible_tag_on_screen) == "function" then
			return a.minimize_visible_tag_on_screen(focused_screen())
		end

		return false
	end)
end

local function toggle_all_tags_on_screen(actions)
	toggle_scope_minimize(actions, function(a)
		if a and type(a.minimize_all_tags_on_screen) == "function" then
			return a.minimize_all_tags_on_screen(focused_screen())
		end

		return false
	end)
end

local function toggle_visible_tags_on_all_screens(actions)
	toggle_scope_minimize(actions, function(a)
		if a and type(a.minimize_visible_tags_on_all_screens) == "function" then
			return a.minimize_visible_tags_on_all_screens()
		end

		return false
	end)
end

local function toggle_all_tags_on_all_screens(actions)
	toggle_scope_minimize(actions, function(a)
		if a and type(a.minimize_all_tags_on_all_screens) == "function" then
			return a.minimize_all_tags_on_all_screens()
		end

		return false
	end)
end

local function toggle_layout_state(actions, cfg)
	local c = focused_client()
	if not c then
		return
	end

	if actions and type(actions.toggle_layout_state) == "function" then
		actions.toggle_layout_state(c, cfg)
		return
	end

	awful.client.floating.toggle(c)
	c:raise()
end

local function layout_state_desc(actions, cfg)
	if actions and type(actions.layout_state_mode) == "function" then
		return (actions.layout_state_mode(cfg) == "maximized") and "Toggle Maximized" or "Toggle Floating"
	end

	return "Toggle Floating"
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(modkey, cfg, actions)
	actions = actions or {}

	return awful.util.table.join(
		awful.key({ modkey }, "f", function()
			toggle_native_fullscreen()
		end, {
			description = "Toggle Fullscreen",
			group = "Client",
		}),

		awful.key({ modkey }, "m", function()
			toggle_minimized(actions)
		end, {
			description = "Minimize Or Restore",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "m", function()
			toggle_visible_tag_on_screen(actions)
		end, {
			description = "Minimize Visible Tag Or Restore",
			group = "Client",
		}),

		awful.key({ modkey, "Control" }, "m", function()
			toggle_all_tags_on_screen(actions)
		end, {
			description = "Minimize Screen Or Restore",
			group = "Client",
		}),

		awful.key({ modkey, "Shift", "Mod1" }, "m", function()
			toggle_visible_tags_on_all_screens(actions)
		end, {
			description = "Minimize Visible Tags On All Screens Or Restore",
			group = "Client",
		}),

		awful.key({ modkey, "Control", "Mod1" }, "m", function()
			toggle_all_tags_on_all_screens(actions)
		end, {
			description = "Minimize All Screens Or Restore",
			group = "Client",
		}),

		awful.key({ modkey }, "t", function()
			toggle_layout_state(actions, cfg)
		end, {
			description = layout_state_desc(actions, cfg),
			group = "Client",
		})
	)
end

return function(modkey, cfg, actions)
	return M.build(modkey, cfg, actions)
end
