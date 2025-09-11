-- features/shell/menu/popup.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local Popup = {}

-- args = { header, cols, footer, theme, placement }  -- placement: fn(p, s, opts)
function Popup.build(args)
	local header = args.header
	local cols = args.cols
	local footer = args.footer
	local t = args.theme or {}
	local place_fn = args.placement
		or function(p, s) -- Fallback: unten links
			local g = s.geometry
			p.x = g.x
			p.y = g.y + g.height - (t.total_height or 520)
		end

	-- nicht "root" nennen, um Awesome's global root nicht zu überschreiben
	local container = wibox.widget({
		header,
		cols,
		footer,
		layout = wibox.layout.align.vertical,
	})

	local popup = awful.popup({
		widget = container,
		visible = false,
		ontop = true,
		shape = t.shape or gears.shape.rectangle,
		border_width = t.border_width or 1,
		border_color = t.border_color or "#000000",
		bg = t.bg or "#222222",
		fg = t.fg or "#ffffff",
		minimum_width = (t.col_left_w or 320) + (t.col_right_w or 220) + (t.cols_pad_l or 6) + (t.cols_pad_r or 6),
		minimum_height = t.total_height or 520,
	})

	-- Outside-Click: globale root.buttons temporär erweitern
	local saved_root_buttons = nil

	local function start_root_click_close(api)
		if saved_root_buttons ~= nil then
			return
		end
		saved_root_buttons = root.buttons()
		local closer = gears.table.join(
			awful.button({}, 1, function()
				api:hide()
			end),
			awful.button({}, 3, function()
				api:hide()
			end)
		)
		if saved_root_buttons and #saved_root_buttons > 0 then
			root.buttons(gears.table.join(saved_root_buttons, closer))
		else
			root.buttons(closer)
		end
	end

	local function stop_root_click_close()
		if saved_root_buttons ~= nil then
			root.buttons(saved_root_buttons)
			saved_root_buttons = nil
		end
	end

	-- Öffentliche API
	local api = {}

	-- opts: { coords={x=...}, screen=<screen> }
	function api:show(opts)
		local s = (opts and opts.screen) or mouse.screen or awful.screen.focused()
		popup.screen = s
		-- Platzierung: darf optional opts lesen (für with_cursor_x)
		place_fn(popup, s, opts)
		popup.visible = true
		start_root_click_close(api)
	end

	function api:hide()
		popup.visible = false
		stop_root_click_close()
	end

	function api:toggle(opts)
		if popup.visible then
			self:hide()
		else
			self:show(opts)
		end
	end

	popup:connect_signal("property::visible", function()
		if not popup.visible then
			stop_root_click_close()
		end
	end)

	return api
end

return Popup
