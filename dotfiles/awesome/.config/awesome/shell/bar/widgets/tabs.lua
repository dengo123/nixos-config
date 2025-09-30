-- ~/.config/awesome/shell/bar/widgets/tabs.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

local function ellipsize(txt, max)
	if not txt then
		return ""
	end
	if #txt <= max then
		return txt
	end
	return txt:sub(1, math.max(0, max - 1)) .. "…"
end

function M.build(s, opts)
	opts = opts or {}
	local modkey = opts.modkey or "Mod4"
	local spacing = opts.spacing or 6
	local tab_radius = opts.tab_radius or beautiful.border_radius or 6
	local max_title_len = opts.max_title_len or 18
	local icon_size = opts.icon_size or 16
	local pad_h = opts.pad_h or 8
	local pad_v = opts.pad_v or 3

	local tasklist_buttons = gears.table.join(
		awful.button({}, 1, function(c)
			if c == client.focus then
				c.minimized = true
			else
				c:emit_signal("request::activate", "tasklist", { raise = true })
			end
		end),
		awful.button({}, 3, function(c)
			c:emit_signal("request::activate", "tasklist", { raise = true })
			awful.menu.client_list({ theme = { width = 300 } })
		end),
		awful.button({}, 4, function()
			awful.client.focus.byidx(1)
		end),
		awful.button({}, 5, function()
			awful.client.focus.byidx(-1)
		end)
	)

	local tasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags, -- NUR Tasks
		buttons = tasklist_buttons,

		layout = {
			spacing = spacing,
			layout = wibox.layout.fixed.horizontal,
		},

		widget_template = {
			{
				{
					{
						id = "icon_role",
						widget = wibox.widget.imagebox,
						resize = true,
						forced_height = icon_size,
						forced_width = icon_size,
					},
					{
						id = "title_role",
						widget = wibox.widget.textbox,
						ellipsize = "end",
					},
					layout = wibox.layout.fixed.horizontal,
					spacing = 6,
				},
				widget = wibox.container.margin,
				left = pad_h,
				right = pad_h,
				top = pad_v,
				bottom = pad_v,
			},
			id = "bg_role",
			widget = wibox.container.background,
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, tab_radius)
			end,

			create_callback = function(self, c, _idx, _objs)
				self._icon = self:get_children_by_id("icon_role")[1]
				self._title = self:get_children_by_id("title_role")[1]
				-- Initial
				if self._icon then
					self._icon.image = c.icon or nil
				end
				if self._title then
					local txt = c.class or c.name or "App"
					self._title.markup = ellipsize(txt, max_title_len)
				end
			end,

			update_callback = function(self, c, _idx, _objs)
				if self._icon then
					self._icon.image = c.icon or nil
				end
				if self._title then
					local txt = c.class or c.name or "App"
					self._title.markup = ellipsize(txt, max_title_len)
				end

				-- Zustände einfärben
				local bg, fg
				if c == client.focus then
					bg = beautiful.tasklist_bg_focus or beautiful.bg_focus or "#4C6EF5"
					fg = beautiful.tasklist_fg_focus or beautiful.fg_focus or "#FFFFFF"
				elseif c.minimized then
					bg = beautiful.tasklist_bg_minimize or "#00000000"
					fg = beautiful.tasklist_fg_minimize or (beautiful.fg_minimize or "#AAAAAA")
				else
					bg = beautiful.tasklist_bg_normal or "#00000000"
					fg = beautiful.tasklist_fg_normal or (beautiful.fg_normal or "#DDDDDD")
				end
				local bgw = self:get_children_by_id("bg_role")[1]
				if bgw then
					bgw.bg, bgw.fg = bg, fg
				end
			end,
		},
	})

	return { tasklist = tasklist }
end

return M
