-- ~/.config/awesome/input/keys/global/state.lua
local awful = require("awful")

return function(modkey, cfg)
	local windowing_actions = (((cfg or {}).actions or {}).windowing or {}).clients or {}

	-- =========================================================================
	-- Helpers
	-- =========================================================================

	local function toggle_layout_state()
		local c = client.focus
		if not c then
			return
		end

		if type(windowing_actions.toggle_layout_state) == "function" then
			windowing_actions.toggle_layout_state(c, cfg)
			return
		end

		awful.client.floating.toggle(c)
		c:raise()
	end

	local layout_state_desc = "toggle floating"

	if type(windowing_actions.layout_state_mode) == "function" then
		layout_state_desc = (windowing_actions.layout_state_mode(cfg) == "maximized") and "toggle maximized"
			or "toggle floating"
	end

	-- =========================================================================
	-- Public API
	-- =========================================================================

	return awful.util.table.join(
		-- ---------------------------------------------------------------------
		-- Screen
		-- ---------------------------------------------------------------------

		awful.key({ modkey }, "F5", function()
			awful.spawn("autorandr-toggle", false)
		end, {
			description = "toggle monitor layout (autorandr)",
			group = "Screen",
		}),

		-- ---------------------------------------------------------------------
		-- Fullscreen
		-- ---------------------------------------------------------------------

		awful.key({ modkey }, "f", function()
			local c = client.focus
			if not c then
				return
			end

			local entering = not c.fullscreen
			c._fullscreen_dim = false
			c.fullscreen = entering
			c:raise()

			if not entering then
				c._fullscreen_dim = nil
			end
		end, {
			description = "toggle fullscreen (native)",
			group = "Client",
		}),

		awful.key({ modkey, "Shift" }, "f", function()
			local c = client.focus
			if not c then
				return
			end

			local entering = not c.fullscreen
			c._fullscreen_dim = true
			c.fullscreen = entering
			c:raise()

			if not entering then
				c._fullscreen_dim = nil
			end
		end, {
			description = "toggle fullscreen (dim other screens)",
			group = "Client",
		}),

		-- ---------------------------------------------------------------------
		-- Client State
		-- ---------------------------------------------------------------------

		awful.key({ modkey }, "m", function()
			local c = client.focus
			if c and not c.minimized then
				awesome.emit_signal("ui::suppress_center", 0.2)
				c.minimized = true
			else
				awesome.emit_signal("windowing::restore_request", awful.screen.focused())
			end
		end, {
			description = "minimize / restore",
			group = "Client",
		}),

		awful.key({ modkey }, "t", toggle_layout_state, {
			description = layout_state_desc,
			group = "Client",
		})
	)
end
