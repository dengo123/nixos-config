local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.register(args)
	local cfg = args.cfg
	local is_ready = args.is_ready
	local set_ready = args.set_ready
	local set_visible = args.set_visible
	local each_popup = args.each_popup
	local rebuild_popup = args.rebuild_popup
	local apply_geometry = args.apply_geometry
	local sync_popups = args.sync_popups
	local scroll_up = args.scroll_up
	local scroll_down = args.scroll_down
	local before_history_rebuild = args.before_history_rebuild
	local after_history_rebuild = args.after_history_rebuild

	if is_ready() then
		return
	end

	set_ready(true)

	awesome.connect_signal("notify::center_state", function(open)
		set_visible(open == true)
	end)

	awesome.connect_signal("notify::history_changed", function()
		each_popup(function(popup)
			if popup and popup.valid then
				if type(before_history_rebuild) == "function" then
					before_history_rebuild(popup)
				end

				rebuild_popup(popup)
				apply_geometry(popup)

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

	screen.connect_signal("property::geometry", function()
		sync_popups()
	end)

	if cfg.notify.center.close_on_tag_switch == true then
		tag.connect_signal("property::selected", function()
			awesome.emit_signal("notify::close_center")
		end)
	end

	if cfg.notify.center.close_on_client_focus == true then
		client.connect_signal("focus", function()
			awesome.emit_signal("notify::close_center")
		end)
	end
end

return M
