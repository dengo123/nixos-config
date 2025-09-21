-- ~/.config/awesome/features/shell/widgets/tabs.lua
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

-- wählt ein "Lead"-Fenster pro Tag:
-- 1) fokussiertes Fenster auf dem Tag
-- 2) sonst erstes nicht-minimiertes
-- 3) sonst das erste beliebige
local function pick_lead_client(t)
	local cls = t:clients()
	if #cls == 0 then
		return nil, {}
	end

	-- fokussiertes auf diesem Tag?
	if client.focus and client.focus.first_tag == t then
		-- restliche ohne lead
		local rest = {}
		for _, c in ipairs(cls) do
			if c ~= client.focus then
				table.insert(rest, c)
			end
		end
		return client.focus, rest
	end

	-- erstes nicht-minimiertes
	for i, c in ipairs(cls) do
		if not c.minimized then
			local rest = gears.table.clone(cls)
			table.remove(rest, i)
			return c, rest
		end
	end

	-- fallback: erstes
	local lead = cls[1]
	table.remove(cls, 1)
	return lead, cls
end

function M.build(s, opts)
	opts = opts or {}
	local modkey = opts.modkey or "Mod4"
	local max_title_len = opts.max_title_len or 14
	local mini_icon_size = opts.mini_icon_size or 14
	local spacing = opts.spacing or 6
	local tag_radius = opts.tag_radius or beautiful.border_radius or 6

	local taglist_buttons = gears.table.join(
		awful.button({}, 1, function(t)
			t:view_only()
		end),
		awful.button({ modkey }, 1, function(t)
			if client.focus then
				client.focus:move_to_tag(t)
			end
		end),
		awful.button({}, 3, awful.tag.viewtoggle),
		awful.button({ modkey }, 3, function(t)
			if client.focus then
				client.focus:toggle_tag(t)
			end
		end),
		awful.button({}, 4, function(t)
			awful.tag.viewnext(t.screen)
		end),
		awful.button({}, 5, function(t)
			awful.tag.viewprev(t.screen)
		end)
	)

	local taglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,

		layout = {
			spacing = spacing,
			layout = wibox.layout.fixed.horizontal,
		},

		style = {
			shape = gears.shape.rounded_rect,
		},

		widget_template = {
			{
				{
					-- Lead: Icon + Titel
					{
						{
							id = "lead_icon_role",
							widget = wibox.widget.imagebox,
							resize = true,
							forced_height = 16,
							forced_width = 16,
						},
						{
							id = "lead_title_role",
							widget = wibox.widget.textbox,
						},
						layout = wibox.layout.fixed.horizontal,
						spacing = 6,
					},
					-- Extras: kleine Icons der übrigen Clients
					{
						id = "extras_role",
						layout = wibox.layout.fixed.horizontal,
						spacing = 4,
					},
					layout = wibox.layout.fixed.horizontal,
					spacing = 8,
				},
				id = "margin_role",
				widget = wibox.container.margin,
				left = 8,
				right = 8,
				top = 3,
				bottom = 3,
			},
			id = "background_role",
			widget = wibox.container.background,
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, tag_radius)
			end,

			-- called when widget is created
			create_callback = function(self, t, _index, _objects)
				-- store roles for updates
				self._lead_icon = self:get_children_by_id("lead_icon_role")[1]
				self._lead_title = self:get_children_by_id("lead_title_role")[1]
				self._extras = self:get_children_by_id("extras_role")[1]
			end,

			-- called on every refresh
			update_callback = function(self, t, _index, _objects)
				local lead, rest = pick_lead_client(t)
				local lead_icon = self._lead_icon
				local lead_title = self._lead_title
				local extras = self._extras

				-- Extras leeren
				extras:reset()

				if not lead then
					-- LEER: zeige Tag-Index als Zahl
					lead_icon.image = nil
					lead_title.markup = string.format("<b>%s</b>", t.name or tostring(t.index))
					return
				end

				-- Lead Icon
				if lead.icon then
					lead_icon.image = lead.icon
				else
					lead_icon.image = nil
				end

				-- Lead Title (kurz)
				local title = lead.class or lead.name or "App"
				lead_title.markup = ellipsize(title, max_title_len)

				-- Restliche Clients: Mini-Icons anhängen
				for _, c in ipairs(rest) do
					local ib = wibox.widget({
						widget = wibox.widget.imagebox,
						resize = true,
						forced_height = mini_icon_size,
						forced_width = mini_icon_size,
						image = c.icon,
					})
					extras:add(ib)
				end
			end,
		},
	})

	return { taglist = taglist }
end

return M
