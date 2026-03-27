-- ~/.config/awesome/shell/notify/center/signals.lua
local M = {}

local runtime = {
	ctx = {},
	registered = false,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function center_cfg(cfg)
	return ((cfg or {}).notify or {}).center or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	return M
end

function M.register(args)
	args = args or {}

	local cfg = args.cfg or ctx().cfg or {}
	local center = center_cfg(cfg)

	local is_ready = args.is_ready
	local set_ready = args.set_ready
	local set_visible = args.set_visible
	local each_popup = args.each_popup
	local rebuild_popup = args.rebuild_popup
	local apply_geometry = args.apply_geometry
	local sync_popups = args.sync_popups
	local select_prev = args.select_prev
	local select_next = args.select_next
	local scroll_up = args.scroll_up
	local scroll_down = args.scroll_down
	local activate_selected = args.activate_selected
	local dismiss_selected = args.dismiss_selected
	local clear_history = args.clear_history
	local before_history_rebuild = args.before_history_rebuild
	local after_history_rebuild = args.after_history_rebuild

	if type(is_ready) == "function" and is_ready() then
		return
	end

	if type(set_ready) == "function" then
		set_ready(true)
	end

	runtime.registered = true

	awesome.connect_signal("notify::center_state", function(open)
		if type(set_visible) == "function" then
			set_visible(open == true)
		end
	end)

	awesome.connect_signal("notify::history_changed", function()
		if type(each_popup) ~= "function" then
			return
		end

		each_popup(function(popup)
			if popup and popup.valid then
				if type(before_history_rebuild) == "function" then
					before_history_rebuild(popup)
				end

				if type(rebuild_popup) == "function" then
					rebuild_popup(popup)
				end

				if type(apply_geometry) == "function" then
					apply_geometry(popup)
				end

				if type(after_history_rebuild) == "function" then
					after_history_rebuild(popup)
				end
			end
		end)
	end)

	awesome.connect_signal("notify::center_scroll_up", function()
		if type(scroll_up) == "function" then
			scroll_up()
		end
	end)

	awesome.connect_signal("notify::center_scroll_down", function()
		if type(scroll_down) == "function" then
			scroll_down()
		end
	end)

	awesome.connect_signal("notify::center_select_prev", function()
		if type(select_prev) == "function" then
			select_prev()
		end
	end)

	awesome.connect_signal("notify::center_select_next", function()
		if type(select_next) == "function" then
			select_next()
		end
	end)

	awesome.connect_signal("notify::center_activate_selected", function()
		if type(activate_selected) == "function" then
			activate_selected()
		end
	end)

	awesome.connect_signal("notify::center_dismiss_selected", function()
		if type(dismiss_selected) == "function" then
			dismiss_selected()
		end
	end)

	awesome.connect_signal("notify::center_clear_history", function()
		if type(clear_history) == "function" then
			clear_history()
		end
	end)

	screen.connect_signal("property::geometry", function()
		if type(sync_popups) == "function" then
			sync_popups()
		end
	end)

	if center.close_on_tag_switch == true then
		tag.connect_signal("property::selected", function()
			awesome.emit_signal("notify::close_center")
		end)
	end

	if center.close_on_client_focus == true then
		client.connect_signal("focus", function()
			awesome.emit_signal("notify::close_center")
		end)
	end
end

return M
