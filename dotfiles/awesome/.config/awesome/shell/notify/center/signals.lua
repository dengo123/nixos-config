-- ~/.config/awesome/shell/notify/center/signals.lua
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

	if is_ready() then
		return
	end

	set_ready(true)

	awesome.connect_signal("notify::center_state", function(open)
		set_visible(open == true)
	end)

	awesome.connect_signal("notify::history_changed", function()
		each_popup(function(popup)
			if popup.box and popup.box.valid then
				apply_geometry(popup)
				rebuild_popup(popup)
			end
		end)
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
