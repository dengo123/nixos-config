-- ~/.config/awesome/shell/notify/center/signals.lua
local awful = require("awful")

local M = {}

local runtime = {
	keygrabber = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function center_cfg(cfg)
	return ((cfg or {}).notify or {}).center or {}
end

local function stop_modal_keys()
	local kg = runtime.keygrabber
	runtime.keygrabber = nil

	if kg and type(kg.stop) == "function" then
		pcall(function()
			kg:stop()
		end)
	end
end

local function has_modifier(mods)
	return mods and #mods > 0
end

local function start_modal_keys(args)
	stop_modal_keys()

	runtime.keygrabber = awful.keygrabber({
		autostart = true,
		stop_event = "release",

		keypressed_callback = function(_, mods, key)
			mods = mods or {}

			local ctrl = false
			for _, m in ipairs(mods) do
				if m == "Control" then
					ctrl = true
					break
				end
			end

			if not has_modifier(mods) and key == "Escape" then
				awesome.emit_signal("notify::close_center")
				return
			end

			if not has_modifier(mods) and key == "Up" then
				if type(args.select_prev) == "function" then
					args.select_prev()
				end
				return
			end

			if not has_modifier(mods) and key == "Down" then
				if type(args.select_next) == "function" then
					args.select_next()
				end
				return
			end

			if not has_modifier(mods) and key == "Prior" then
				if type(args.scroll_up) == "function" then
					args.scroll_up()
				end
				return
			end

			if not has_modifier(mods) and key == "Next" then
				if type(args.scroll_down) == "function" then
					args.scroll_down()
				end
				return
			end

			if not has_modifier(mods) and key == "Return" then
				if type(args.activate_selected) == "function" then
					args.activate_selected()
				end
				return
			end

			if not has_modifier(mods) and key == "BackSpace" then
				if type(args.dismiss_selected) == "function" then
					args.dismiss_selected()
				end
				return
			end

			if ctrl and key == "Delete" then
				if type(args.clear_history) == "function" then
					args.clear_history()
				end
				return
			end
		end,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.register(args)
	args = args or {}

	local cfg = args.cfg or {}
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

	awesome.connect_signal("notify::center_state", function(open)
		if type(set_visible) == "function" then
			set_visible(open == true)
		end

		if open == true then
			start_modal_keys({
				select_prev = select_prev,
				select_next = select_next,
				scroll_up = scroll_up,
				scroll_down = scroll_down,
				activate_selected = activate_selected,
				dismiss_selected = dismiss_selected,
				clear_history = clear_history,
			})
		else
			stop_modal_keys()
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
