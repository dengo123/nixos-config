-- ~/.config/awesome/widgets/xpbar.lua
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local menubar = require("menubar")

local dpi = beautiful.xresources and beautiful.xresources.apply_dpi or function(x)
	return x
end

local M = {}

-- kleine Helper: versuche Rofi, sonst Awesome-Menü
local function launch_menu()
	awful.spawn.easy_async_with_shell("command -v rofi >/dev/null 2>&1; echo $?", function(out)
		if tostring(out or ""):match("0") then
			awful.spawn("rofi -show drun")
		else
			menubar.show()
		end
	end)
end

-- Tasklist-Buttons: Linksklick -> zum Tag des Clients wechseln, Fokus + raise
local tasklist_buttons = gears.table.join(
	awful.button({}, 1, function(c)
		local t = c.first_tag
		if t then
			t:view_only()
		end
		client.focus = c
		c:raise()
	end),
	awful.button({}, 3, function(c)
		if c == client.focus then
			c.minimized = true
		else
			local t = c.first_tag
			if t then
				t:view_only()
			end
			client.focus = c
			c:raise()
		end
	end),
	awful.button({}, 2, function(c)
		c:kill()
	end)
)

-- Quelle für Tasklist: alle Clients des Screens, sortiert nach Tag-Index, dann Titel
local function tasklist_source(s)
	local cls = {}
	for _, c in ipairs(client.get(s)) do
		-- nur Clients, die Tags auf diesem Screen haben (ignoriere Sticky/ohne Tag)
		local t = c.first_tag
		if t and t.screen == s then
			table.insert(cls, c)
		end
	end
	table.sort(cls, function(a, b)
		local ta = (a.first_tag and a.first_tag.index) or 999
		local tb = (b.first_tag and b.first_tag.index) or 999
		if ta == tb then
			return (a.name or ""):lower() < (b.name or ""):lower()
		else
			return ta < tb
		end
	end)
	return cls
end

-- kleiner Farbstreifen je nach Tag (statt „Tag-Zahl“)
local function tag_strip_widget(c)
	local idx = (c.first_tag and c.first_tag.index) or 1
	-- einfache deterministische Farbwahl aus Theme-Basis
	local base = beautiful.xp_tag_colors
		or {
			"#5fa5ff",
			"#9ccc65",
			"#ffd54f",
			"#ff8a65",
			"#ba68c8",
			"#4db6ac",
			"#64b5f6",
			"#81c784",
			"#fff176",
			"#ffab91",
		}
	local color = base[((idx - 1) % #base) + 1]
	return wibox.widget({
		forced_width = dpi(4),
		forced_height = dpi(22),
		widget = wibox.widget.separator,
		color = color,
		span_ratio = 1.0,
		thickness = dpi(4),
	})
end

-- XP-ähnlicher Task-Tab (Icon + Titel, runde Ecken)
local function task_template()
	return {
		{
			{
				-- linker Farbstreifen je nach Tag
				{
					id = "tag_strip_role",
					widget = wibox.container.background, -- wird in update überschrieben
					forced_width = dpi(0), -- placeholder
				},
				-- Icon + Titel
				{
					{
						{
							id = "icon_role",
							widget = wibox.widget.imagebox,
							resize = true,
						},
						{
							id = "text_role",
							widget = wibox.widget.textbox,
							ellipsize = "end",
						},
						spacing = dpi(6),
						layout = wibox.layout.fixed.horizontal,
					},
					left = dpi(10),
					right = dpi(12),
					top = dpi(3),
					bottom = dpi(3),
					widget = wibox.container.margin,
				},
				spacing = dpi(8),
				layout = wibox.layout.fixed.horizontal,
			},
			widget = wibox.container.background,
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, dpi(6))
			end,
			id = "background_role",
		},
		left = dpi(4),
		right = dpi(4),
		widget = wibox.container.margin,
	}
end

-- Start-Button (nur primary screen)
local function start_button_widget()
	local text = wibox.widget({
		widget = wibox.widget.textbox,
		markup = "<b>Start</b>",
		valign = "center",
		align = "center",
	})
	local btn = wibox.widget({
		{
			{
				-- optional Icon links vom Text:
				-- { image = beautiful.awesome_icon, widget = wibox.widget.imagebox, resize=true },
				-- spacing = dpi(6),
				text,
				layout = wibox.layout.fixed.horizontal,
			},
			left = dpi(12),
			right = dpi(12),
			top = dpi(3),
			bottom = dpi(3),
			widget = wibox.container.margin,
		},
		bg = beautiful.xp_start_bg or "#4CAF50",
		fg = beautiful.xp_start_fg or beautiful.fg_focus or "#ffffff",
		shape = function(cr, w, h)
			gears.shape.rounded_bar(cr, w, h)
		end,
		widget = wibox.container.background,
	})
	btn:buttons(gears.table.join(awful.button({}, 1, launch_menu)))
	return btn
end

function M.create_for_screen(s)
	local is_primary = (s == screen.primary)

	-- Tasklist über alle Tags des Screens (XP-Gefühl), Klick wechselt Tag + fokussiert
	local tasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.alltags,
		buttons = tasklist_buttons,
		source = function()
			return tasklist_source(s)
		end,
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(6),
		widget_template = task_template(),
		style = {
			shape_border_width = dpi(0),
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, dpi(6))
			end,
		},
	})

	-- update hook für den farbigen Tag-Streifen
	tasklist._original_update = tasklist._original_update or tasklist._private.refresh
	tasklist._private.refresh = function(...)
		tasklist._original_update(...)
		-- gehe durch die Buttons und färbe den Streifen passend zum Tag
		for _, btn in ipairs(tasklist._private.tasklist_buttons or {}) do
			local c = btn._client
			if c then
				local strip = btn:get_children_by_id("tag_strip_role")[1]
				if strip then
					local idx = (c.first_tag and c.first_tag.index) or 1
					local base = beautiful.xp_tag_colors
						or {
							"#5fa5ff",
							"#9ccc65",
							"#ffd54f",
							"#ff8a65",
							"#ba68c8",
							"#4db6ac",
							"#64b5f6",
							"#81c784",
							"#fff176",
							"#ffab91",
						}
					local color = base[((idx - 1) % #base) + 1]
					strip.bg = color
					strip.forced_width = dpi(4)
				end
			end
		end
	end

	-- systray nur auf primary
	local tray = nil
	if is_primary then
		tray = wibox.widget.systray()
		tray:set_base_size(dpi(18))
	end

	local left_widgets = {
		layout = wibox.layout.fixed.horizontal,
	}
	if is_primary then
		table.insert(left_widgets, start_button_widget())
	end

	local right_widgets = {
		layout = wibox.layout.fixed.horizontal,
	}
	if is_primary and tray then
		table.insert(right_widgets, wibox.container.margin(tray, dpi(8), dpi(8), dpi(2), dpi(2)))
	end

	-- Wibar unten
	s.xpbar = awful.wibar({
		screen = s,
		position = "bottom",
		height = dpi(30),
		bg = beautiful.xp_bar_bg or beautiful.bg_normal,
		fg = beautiful.xp_bar_fg or beautiful.fg_normal,
	})

	s.xpbar:setup({
		layout = wibox.layout.align.horizontal,
		left_widgets,
		tasklist,
		right_widgets,
	})
end

return M
