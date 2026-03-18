local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.ensure(popups, key_for_screen, s)
	local key = key_for_screen(s)
	local popup = popups[key]

	if popup and popup.valid then
		return popup
	end

	popup = wibox({
		visible = false,
		ontop = true,
		screen = s,
		bg = "#00000000",
		type = "utility",
	})

	popups[key] = popup

	return popup
end

function M.rebuild(popup, build_panel)
	if not popup or not popup.valid then
		return false
	end

	local panel = build_panel()

	if not panel then
		popup.widget = nil
		popup.visible = false
		return false
	end

	popup:setup({
		panel,
		layout = wibox.layout.fixed.vertical,
	})

	return true
end

local function safe_extra_height()
	return 8
end

function M.apply_geometry(popup, geo)
	if not popup or not popup.valid or not geo then
		return
	end

	if not popup.widget or geo.height <= 0 then
		popup.visible = false
		return
	end

	popup.width = geo.width
	popup.height = geo.height
	popup.x = geo.x
	popup.y = geo.y
end

function M.sync(args)
	local popups = args.popups or {}
	local screens = args.screens or {}
	local key_for_screen = args.key_for_screen
	local ensure_popup = args.ensure_popup
	local rebuild_popup = args.rebuild_popup
	local apply_geometry = args.apply_geometry

	local wanted = {}

	for _, s in ipairs(screens) do
		local key = key_for_screen(s)
		wanted[key] = true

		local popup = ensure_popup(s)
		local has_content = rebuild_popup(popup)

		if has_content then
			apply_geometry(popup)
		else
			popup.visible = false
		end
	end

	for key, popup in pairs(popups) do
		if not wanted[key] and popup and popup.valid then
			popup.visible = false
		end
	end
end

function M.set_visible(args, open)
	local popups = args.popups or {}
	local screens = args.screens or {}
	local key_for_screen = args.key_for_screen
	local sync_popups = args.sync_popups
	local on_open = args.on_open
	local on_close = args.on_close

	sync_popups()

	local wanted = {}
	for _, s in ipairs(screens) do
		wanted[key_for_screen(s)] = true
	end

	for key, popup in pairs(popups) do
		if popup and popup.valid then
			local has_widget = popup.widget ~= nil
			popup.visible = (open == true) and wanted[key] == true and has_widget
		end
	end

	if open == true then
		if type(on_open) == "function" then
			on_open()
		end
	else
		if type(on_close) == "function" then
			on_close()
		end
	end
end

return M
