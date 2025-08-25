-- ~/.config/awesome/widgets/tasklist.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local dpi = require("beautiful.xresources").apply_dpi

local M = {}

function M.tasklist(s)
	return awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		layout = { spacing = dpi(6), layout = wibox.layout.flex.horizontal },
		style = {
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, dpi(3))
			end,
		},
		widget_template = {
			{
				{
					{ id = "icon_role", widget = wibox.widget.imagebox },
					{ id = "text_role", widget = wibox.widget.textbox },
					spacing = dpi(6),
					layout = wibox.layout.fixed.horizontal,
				},
				left = dpi(10),
				right = dpi(10),
				top = dpi(3),
				bottom = dpi(3),
				widget = wibox.container.margin,
			},
			id = "background_role",
			widget = wibox.container.background,
		},
	})
end

-- „XP‑Taskbuttons“ – aber pro Tag (≙ nach Tag gruppiert)
function M.tag_taskbuttons(s)
	-- Taglist, die wie Taskbuttons aussieht
	return awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		layout = { spacing = dpi(6), layout = wibox.layout.fixed.horizontal },
		style = {
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, dpi(3))
			end,
		},
		widget_template = {
			{
				{
					-- Icon + Name + (Anzahl Fenster im Tag)
					{
						id = "text_role",
						widget = wibox.widget.textbox,
					},
					{
						id = "clients_count",
						widget = wibox.widget.textbox,
					},
					spacing = dpi(6),
					layout = wibox.layout.fixed.horizontal,
				},
				left = dpi(10),
				right = dpi(10),
				top = dpi(3),
				bottom = dpi(3),
				widget = wibox.container.margin,
			},
			id = "background_role",
			widget = wibox.container.background,

			create_callback = function(self, t, _)
				-- Fensteranzahl im Tag anzeigen, z. B. "3"
				local count = function()
					local n = 0
					for _, c in ipairs(t:clients()) do
						if not c.skip_taskbar then
							n = n + 1
						end
					end
					return n
				end
				local cc = self:get_children_by_id("clients_count")[1]
				cc.markup = ""

				local update = function()
					local n = count()
					cc.markup = n > 0 and (" (" .. n .. ")") or ""
				end
				update()
				-- refresh bei Änderungen
				t:connect_signal("property::selected", update)
				t:connect_signal("tagged", update)
				t:connect_signal("untagged", update)
				client.connect_signal("manage", update)
				client.connect_signal("unmanage", update)
			end,
		},
		buttons = gears.table.join(
			awful.button({}, 1, function(t)
				t:view_only()
			end),
			awful.button({ "Mod4" }, 1, function(t)
				if client.focus then
					client.focus:move_to_tag(t)
				end
			end),
			awful.button({}, 3, awful.tag.viewtoggle)
		),
	})
end

return M
