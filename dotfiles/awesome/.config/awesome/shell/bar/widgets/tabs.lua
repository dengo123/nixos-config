-- ~/.config/awesome/shell/bar/widgets/tabs.lua

local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

-- optionales System-Config (kann z.B. später wieder für Modi genutzt werden)
local syscfg_ok, syscfg = pcall(require, "system.config")
syscfg = syscfg_ok and syscfg or {}

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

-- Lead-Client: fokussiert > erster nicht minimierter > erster
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

-- == Tab-Element =============================================================
-- menu_api: { show_for_widget_with_clients_at(widget, clients, { x_left = <abs> }) }
local function build_group_tab(label, clients, theme, H, FIX_W, menu_api)
	assert(theme and theme.colors, "tabs.lua: theme.colors fehlt")
	assert(theme.icon_size, "tabs.lua: theme.icon_size fehlt")
	assert(theme.pad_h, "tabs.lua: theme.pad_h fehlt")
	assert(theme.pad_v, "tabs.lua: theme.pad_v fehlt")
	assert(theme.radius, "tabs.lua: theme.radius fehlt")
	assert(theme.inactive_border_width, "tabs.lua: theme.inactive_border_width fehlt")
	assert(theme.title_len, "tabs.lua: theme.title_len fehlt")

	local C = theme.colors
	local icon_size = theme.icon_size
	local pad_h = theme.pad_h
	local pad_v = theme.pad_v
	local radius = theme.radius
	local inactive_bw = theme.inactive_border_width
	local title_len = theme.title_len

	local lead = pick_lead(clients)

	-- ICON
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

	-- TITEL
	local title_raw = wibox.widget({
		widget = wibox.widget.textbox,
		markup = ellipsize(label or (lead and (lead.class or lead.name) or "App"), title_len),
	})

	local title = wibox.widget({
		title_raw,
		widget = wibox.container.place,
		valign = "center",
	})

	-- REIHE
	local inner = wibox.widget({
		icon,
		title,
		spacing = 6,
		layout = wibox.layout.fixed.horizontal,
	})

	-- MARGINS
	local with_margin = wibox.container.margin(inner, pad_h, pad_h, pad_v, pad_v)

	-- INHALT
	local content = wibox.widget({
		with_margin,
		widget = wibox.container.place,
		halign = "left",
		valign = "center",
	})

	-- BACKGROUND / BORDER
	local bgw = wibox.widget({
		content,
		widget = wibox.container.background,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		forced_height = H,
		forced_width = FIX_W,
	})

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

		if focused_in_group then
			bgw.bg = C.focus_bg
			bgw.fg = C.focus_fg
			bgw.shape_border_width = 0
			bgw.shape_border_color = C.focus_bg
		else
			bgw.bg = C.normal_bg
			bgw.fg = C.normal_fg
			bgw.shape_border_width = inactive_bw
			bgw.shape_border_color = C.normal_border
		end
	end

	refresh_colors()

	-- Interaktionen
	bgw:buttons(gears.table.join(
		awful.button({}, 1, function()
			-- Linksklick: fokussierten Client minimieren, sonst Lead aktivieren
			local focused = client.focus
			if focused then
				for _, c in ipairs(clients) do
					if c == focused and c.valid then
						c.minimized = true
						return
					end
				end
			end

			local lead_now = (lead and lead.valid) and lead or pick_lead(clients)
			if lead_now and lead_now.valid then
				lead_now:emit_signal("request::activate", "group_tab", { raise = true })
			end
		end),

		awful.button({}, 4, function()
			awful.client.focus.byidx(1)
		end),

		awful.button({}, 5, function()
			awful.client.focus.byidx(-1)
		end)
	))

	-- Rechtsklick: Kontextmenü via zentrales Menu-API
	local has_menu = (type(menu_api) == "table" and type(menu_api.show_for_widget_with_clients_at) == "function")

	bgw:connect_signal("button::press", function(_, lx, _, button)
		if button ~= 3 or not has_menu then
			return
		end

		local target = (lead and lead.valid) and lead or pick_lead(clients)
		if not (target and target.valid) then
			return
		end

		local mc = mouse.coords()
		local x_left = mc.x - (lx or 0)

		-- Liste mit genau einem Client; Items.build_clients macht daraus das Aktionen-Menü
		menu_api.show_for_widget_with_clients_at(bgw, { target }, { x_left = x_left })
	end)

	client.connect_signal("focus", refresh_colors)
	client.connect_signal("unfocus", refresh_colors)

	return bgw
end

-- == Ungruppierte Taskbar (ein Tab pro Client) ===============================
local function build_single_taskbar(s, theme, H, FIX_W, spacing, menu_api)
	local container = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	local t = s.selected_tag
	if not t then
		return container
	end

	for _, c in ipairs(t:clients() or {}) do
		if c.valid and not c.skip_taskbar and c.screen == s then
			local label = c.name or c.class or "App"
			container:add(build_group_tab(label, { c }, theme, H, FIX_W, menu_api))
		end
	end

	return container
end

-- == Public ==================================================================
function M.build(s, opts)
	opts = opts or {}

	local theme = assert(opts.theme, "tabs.lua: opts.theme muss injiziert werden (Theme.tabs.get(...))")
	local menu_api = opts.menu_api -- erwartet: show_for_widget_with_clients_at

	local H = assert(tonumber(beautiful.wibar_height), "tabs.lua: beautiful.wibar_height fehlt/ungueltig")

	assert(theme.width_factor, "tabs.lua: theme.width_factor fehlt")
	assert(theme.spacing, "tabs.lua: theme.spacing fehlt")

	local FIX_W = math.floor(theme.width_factor * H)
	local spacing = theme.spacing

	local box = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	local function refresh()
		box:reset()
		local built = build_single_taskbar(s, theme, H, FIX_W, spacing, menu_api)
		for _, child in ipairs(built.children or {}) do
			box:add(child)
		end
	end

	local function delayed_refresh()
		gears.timer.delayed_call(refresh)
	end

	-- Signals
	client.connect_signal("manage", delayed_refresh)
	client.connect_signal("unmanage", delayed_refresh)
	client.connect_signal("property::minimized", delayed_refresh)
	client.connect_signal("property::name", delayed_refresh)
	client.connect_signal("property::class", delayed_refresh)
	client.connect_signal("tagged", delayed_refresh)
	client.connect_signal("untagged", delayed_refresh)
	tag.connect_signal("property::selected", delayed_refresh)
	screen.connect_signal("arrange", delayed_refresh)

	refresh()

	return {
		tasklist = box,
	}
end

return M
