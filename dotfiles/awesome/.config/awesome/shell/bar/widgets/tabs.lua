-- ~/.config/awesome/shell/bar/widgets/tabs.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local Theme = require("ui.theme") -- NEU

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

	-- Theme holen
	local TabsTheme = (Theme and Theme.tabs and Theme.tabs.get and Theme.tabs.get(opts.theme)) or {}
	local spacing = TabsTheme.spacing or (opts.spacing or 6)
	local tab_radius = TabsTheme.radius or (opts.tab_radius or beautiful.border_radius or 6)
	local max_title_len = TabsTheme.title_len or (opts.max_title_len or 18)
	local icon_size = TabsTheme.icon_size or (opts.icon_size or 16)
	local pad_h = TabsTheme.pad_h or (opts.pad_h or 8)
	local pad_v = TabsTheme.pad_v or (opts.pad_v or 3)
	local inactive_bw = TabsTheme.inactive_border_width or 1
	local C = TabsTheme.colors or {}
	local H = tonumber(beautiful.wibar_height) or 28
	local FIX_W = math.floor((TabsTheme.width_factor or 6) * H) -- 6x Wibar-Höhe

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
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
		layout = { spacing = spacing, layout = wibox.layout.fixed.horizontal },

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

			-- fixe Größe: Höhe = Wibar-Höhe, Breite = 6x Wibar-Höhe
			forced_height = H,
			forced_width = FIX_W,

			create_callback = function(self, c)
				self._icon = self:get_children_by_id("icon_role")[1]
				self._title = self:get_children_by_id("title_role")[1]
				if self._icon then
					self._icon.image = c.icon or nil
				end
				if self._title then
					local txt = c.class or c.name or "App"
					self._title.markup = ellipsize(txt, max_title_len)
				end
			end,

			update_callback = function(self, c)
				if self._icon then
					self._icon.image = c.icon or nil
				end
				if self._title then
					local txt = c.class or c.name or "App"
					self._title.markup = ellipsize(txt, max_title_len)
				end

				local bg, fg, bw, bc
				if c == client.focus then
					bg = (C.focus_bg or beautiful.bg_focus or "#4C6EF5")
					fg = (C.focus_fg or beautiful.fg_focus or "#FFFFFF")
					bw = 0 -- aktiver Tab OHNE Rand
					bc = bg
				elseif c.minimized then
					bg = (C.minimize_bg or "#00000000")
					fg = (C.minimize_fg or beautiful.fg_minimize or "#AAAAAA")
					bw = inactive_bw -- dünner Rand in Fokus-Farbe
					bc = (C.focus_bg or "#4C6EF5")
				else
					bg = (C.normal_bg or "#00000000")
					fg = (C.normal_fg or beautiful.fg_normal or "#DDDDDD")
					bw = inactive_bw
					bc = (C.focus_bg or "#4C6EF5")
				end

				-- Farben + Border anwenden
				self.bg, self.fg = bg, fg
				self.shape_border_width = bw
				self.shape_border_color = bc

				-- Sicherheit: fixe Maße auch bei Theme-Reload
				self.forced_height = H
				self.forced_width = FIX_W
			end,
		},
	})

	return { tasklist = tasklist }
end

return M
