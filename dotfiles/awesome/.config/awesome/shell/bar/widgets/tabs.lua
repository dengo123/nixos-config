-- ~/.config/awesome/shell/bar/widgets/tabs.lua
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local Theme = require("ui.theme")

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

-- Lead-Client der Gruppe bestimmen: fokussiert > erster nicht minimierter > erster
local function pick_lead(clients)
	if #clients == 0 then
		return nil
	end
	if client.focus then
		for _, c in ipairs(clients) do
			if c == client.focus then
				return c
			end
		end
	end
	for _, c in ipairs(clients) do
		if not c.minimized then
			return c
		end
	end
	return clients[1]
end

-- eine Gruppen-Kachel (Tab) bauen
local function build_group_tab(cls, clients, theme, H, FIX_W)
	local C = theme.colors or {}
	local icon_size = theme.icon_size or 16
	local pad_h = theme.pad_h or 8
	local pad_v = theme.pad_v or 3
	local radius = theme.radius or (beautiful.border_radius or 6)
	local inactive_bw = theme.inactive_border_width or 1
	local title_len = theme.title_len or 18

	local lead = pick_lead(clients)

	-- ICON: feste Größe + zentriert
	local icon_img = wibox.widget({
		widget = wibox.widget.imagebox,
		resize = true,
		forced_height = icon_size,
		forced_width = icon_size,
		image = (lead and lead.icon) or nil,
	})

	local icon = wibox.widget({
		icon_img,
		widget = wibox.container.place,
		halign = "center",
		valign = "center",
	})

	-- TITEL: vertikal zentrieren
	local title_raw = wibox.widget({
		widget = wibox.widget.textbox,
		markup = ellipsize(cls or (lead and (lead.class or lead.name) or "App"), title_len),
	})

	local title = wibox.widget({
		title_raw,
		widget = wibox.container.place,
		valign = "center",
	})

	-- Reihe (Icon + Titel)
	local inner = wibox.widget({
		icon,
		title,
		spacing = 6,
		layout = wibox.layout.fixed.horizontal,
	})

	-- Innenabstände
	local with_margin = wibox.container.margin(inner, pad_h, pad_h, pad_v, pad_v)

	-- Gesamter Inhalt: linksbündig, vertikal exakt mittig in der Kachel
	local content = wibox.widget({
		with_margin,
		widget = wibox.container.place,
		halign = "left",
		valign = "center",
	})

	-- Hintergrund / Rahmen / Radius
	local bgw = wibox.widget({
		content,
		widget = wibox.container.background,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		forced_height = H,
		forced_width = FIX_W,
	})

	-- Farben je nach Fokus der Gruppe
	local function refresh_colors()
		local focused_in_group = false
		if client.focus then
			for _, c in ipairs(clients) do
				if c == client.focus then
					focused_in_group = true
					break
				end
			end
		end
		local bg, fg, bw, bc
		if focused_in_group then
			bg = C.focus_bg or beautiful.bg_focus or "#4C6EF5"
			fg = C.focus_fg or beautiful.fg_focus or "#FFFFFF"
			bw = 0
			bc = bg
		else
			bg = C.normal_bg or "#00000000"
			fg = C.normal_fg or beautiful.fg_normal or "#DDDDDD"
			bw = inactive_bw
			bc = C.focus_bg or "#4C6EF5"
		end
		bgw.bg, bgw.fg = bg, fg
		bgw.shape_border_width = bw
		bgw.shape_border_color = bc
	end
	refresh_colors()

	-- Interaktionen
	bgw:buttons(gears.table.join(
		-- Linksklick: wenn Gruppe aktiv (ein Client der Gruppe fokussiert) → minimieren,
		-- sonst Lead der Gruppe fokussieren/raisen
		awful.button({}, 1, function()
			local focused = client.focus
			if focused then
				for _, c in ipairs(clients) do
					if c == focused and c.valid then
						c.minimized = true
						return
					end
				end
			end
			if not (lead and lead.valid) then
				lead = pick_lead(clients)
			end
			if lead and lead.valid then
				lead:emit_signal("request::activate", "group_tab", { raise = true })
			end
		end),

		-- Rechtsklick: Dropdown NUR mit Clients dieser Gruppe; Label bevorzugt c.name
		awful.button({}, 3, function()
			local items = {}
			for _, c in ipairs(clients) do
				local label = c.name or c.class or "App"
				table.insert(items, {
					label,
					function()
						if c.valid then
							c:emit_signal("request::activate", "group_menu", { raise = true })
						end
					end,
					c.icon,
				})
			end
			if #items == 0 then
				return
			end
			if _G.__tabs_tag_menu and _G.__tabs_tag_menu.wibox and _G.__tabs_tag_menu.wibox.valid then
				_G.__tabs_tag_menu:hide()
			end
			_G.__tabs_tag_menu = awful.menu({ items = items, theme = { width = 300 } })
			_G.__tabs_tag_menu:show()
		end),

		awful.button({}, 4, function()
			awful.client.focus.byidx(1)
		end),
		awful.button({}, 5, function()
			awful.client.focus.byidx(-1)
		end)
	))

	-- Live-Farbanpassung nur lokal (kein globales Rebuild)
	client.connect_signal("focus", refresh_colors)
	client.connect_signal("unfocus", refresh_colors)

	return bgw
end

-- gruppierte Taskbar für Screen s aufbauen
local function build_grouped_taskbar(s, theme, H, FIX_W, spacing)
	local by_class = {}
	for c in
		awful.client.iterate(function(cc)
			if not (cc and cc.valid) then
				return false
			end
			if cc.skip_taskbar then
				return false
			end
			if cc.screen ~= s then
				return false
			end
			local tags = cc:tags() or {}
			for _, t in ipairs(tags) do
				if t.selected then
					return true
				end
			end
			return false
		end)
	do
		local key = c.class or (c.name or "App")
		by_class[key] = by_class[key] or {}
		table.insert(by_class[key], c)
	end

	local container = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	-- stabile Reihenfolge (alphabetisch, case-insensitive)
	local keys = {}
	for cls, _ in pairs(by_class) do
		table.insert(keys, cls or "")
	end
	table.sort(keys, function(a, b)
		return a:lower() < b:lower()
	end)

	for _, cls in ipairs(keys) do
		container:add(build_group_tab(cls, by_class[cls], theme, H, FIX_W))
	end

	return container
end

function M.build(s, opts)
	opts = opts or {}
	-- Theme
	local TabsTheme = (Theme and Theme.tabs and Theme.tabs.get and Theme.tabs.get(opts.theme)) or {}
	local spacing = TabsTheme.spacing or (opts.spacing or 6)
	local H = tonumber(beautiful.wibar_height) or 28
	local FIX_W = math.floor((TabsTheme.width_factor or 6) * H)

	-- Gruppierte Darstellung (immer aktiv)
	local box = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	local function refresh()
		box:reset()
		local grouped = build_grouped_taskbar(s, TabsTheme, H, FIX_W, spacing)
		-- grouped ist ein Container: seine Kinder in unsere Box übernehmen
		for _, child in ipairs(grouped.children or {}) do
			box:add(child)
		end
	end

	local function delayed_refresh()
		gears.timer.delayed_call(refresh)
	end

	-- Rebuild-Trigger: Änderungen an Client-Liste / Tags / Layout
	client.connect_signal("manage", delayed_refresh)
	client.connect_signal("unmanage", delayed_refresh)
	client.connect_signal("property::minimized", delayed_refresh)
	client.connect_signal("property::name", delayed_refresh)
	client.connect_signal("property::class", delayed_refresh)
	client.connect_signal("tagged", delayed_refresh)
	client.connect_signal("untagged", delayed_refresh)
	tag.connect_signal("property::selected", delayed_refresh)
	screen.connect_signal("arrange", delayed_refresh)
	-- wichtig: KEIN refresh auf focus/unfocus → sonst „springt“ die Leiste

	refresh()
	return { tasklist = box }
end

return M
