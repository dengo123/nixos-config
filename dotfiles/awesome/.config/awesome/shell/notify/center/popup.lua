-- ~/.config/awesome/shell/notify/center/popup.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.ensure(popups, key_for_screen, s)
	local key = key_for_screen(s)
	local popup = popups[key]

	if popup and popup.box and popup.box.valid then
		return popup
	end

	local box = wibox({
		visible = false,
		ontop = true,
		screen = s,
		bg = "#00000000",
		type = "utility",
	})

	popup = {
		screen = s,
		box = box,
	}

	popups[key] = popup

	return popup
end

function M.apply_geometry(popup, geo)
	popup.box.x = geo.x
	popup.box.y = geo.y
	popup.box.width = geo.width
	popup.box.height = geo.height
	popup.box.shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, geo.radius or 0)
	end
end

function M.rebuild(popup, build_panel, geo)
	popup.box:setup({
		{
			build_panel(),
			forced_width = geo.width,
			forced_height = geo.height,
			widget = wibox.container.constraint,
		},
		layout = wibox.layout.fixed.vertical,
	})
end

function M.sync(args)
	local popups = args.popups
	local target_screens = args.target_screens
	local key_for_screen = args.key_for_screen
	local ensure_popup = args.ensure_popup
	local rebuild_popup = args.rebuild_popup
	local apply_geometry = args.apply_geometry

	local wanted = {}

	for _, s in ipairs(target_screens()) do
		local key = key_for_screen(s)
		wanted[key] = true

		local popup = ensure_popup(s)
		apply_geometry(popup)
		rebuild_popup(popup)
	end

	for key, popup in pairs(popups) do
		if not wanted[key] and popup.box and popup.box.valid then
			popup.box.visible = false
		end
	end
end

function M.set_visible(args, open)
	local popups = args.popups
	local target_screens = args.target_screens
	local key_for_screen = args.key_for_screen
	local sync_popups = args.sync_popups
	local on_open = args.on_open
	local on_close = args.on_close

	sync_popups()

	local wanted = {}
	for _, s in ipairs(target_screens()) do
		wanted[key_for_screen(s)] = true
	end

	for key, popup in pairs(popups) do
		if popup.box and popup.box.valid then
			popup.box.visible = (open == true) and wanted[key] == true
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
