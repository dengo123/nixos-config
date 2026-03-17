-- ~/.config/awesome/shell/bar/widgets/tabs.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ellipsize(text, max_len)
	if not text then
		return ""
	end

	if #text <= max_len then
		return text
	end

	return text:sub(1, math.max(0, max_len - 1)) .. "…"
end

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

local function require_number(value, name)
	local n = tonumber(value)
	assert(n ~= nil, "tabs.lua: " .. name .. " fehlt/ungueltig")
	return n
end

local function require_table(value, name)
	assert(type(value) == "table", "tabs.lua: " .. name .. " fehlt/ungueltig")
	return value
end

-- =========================================================================
-- Tab Builder
-- =========================================================================

local function build_group_tab(label, clients, theme, bar_height, fixed_width, menu_api)
	local colors = require_table(theme.colors, "theme.colors")

	local icon_size = require_number(theme.icon_size, "theme.icon_size")
	local pad_h = require_number(theme.pad_h, "theme.pad_h")
	local pad_v = require_number(theme.pad_v, "theme.pad_v")
	local radius = require_number(theme.radius, "theme.radius")
	local inactive_border_width = require_number(theme.inactive_border_width, "theme.inactive_border_width")
	local title_len = require_number(theme.title_len, "theme.title_len")

	local lead = pick_lead(clients)

	-- ---------------------------------------------------------------------
	-- Icon
	-- ---------------------------------------------------------------------

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

	-- ---------------------------------------------------------------------
	-- Title
	-- ---------------------------------------------------------------------

	local title_raw = wibox.widget({
		widget = wibox.widget.textbox,
		markup = ellipsize(label or (lead and (lead.class or lead.name) or "App"), title_len),
	})

	local title = wibox.widget({
		title_raw,
		widget = wibox.container.place,
		valign = "center",
	})

	-- ---------------------------------------------------------------------
	-- Layout
	-- ---------------------------------------------------------------------

	local inner = wibox.widget({
		icon,
		title,
		spacing = 6,
		layout = wibox.layout.fixed.horizontal,
	})

	local with_margin = wibox.container.margin(inner, pad_h, pad_h, pad_v, pad_v)

	local content = wibox.widget({
		with_margin,
		widget = wibox.container.place,
		halign = "left",
		valign = "center",
	})

	local bgw = wibox.widget({
		content,
		widget = wibox.container.background,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		forced_height = bar_height,
		forced_width = fixed_width,
	})

	-- ---------------------------------------------------------------------
	-- State
	-- ---------------------------------------------------------------------

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
			bgw.bg = colors.focus_bg
			bgw.fg = colors.focus_fg
			bgw.shape_border_width = 0
			bgw.shape_border_color = colors.focus_bg
		else
			bgw.bg = colors.normal_bg
			bgw.fg = colors.normal_fg
			bgw.shape_border_width = inactive_border_width
			bgw.shape_border_color = colors.normal_border
		end
	end

	refresh_colors()

	-- ---------------------------------------------------------------------
	-- Mouse
	-- ---------------------------------------------------------------------

	bgw:buttons(gears.table.join(
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

		menu_api.show_for_widget_with_clients_at(bgw, { target }, { x_left = x_left })
	end)

	client.connect_signal("focus", refresh_colors)
	client.connect_signal("unfocus", refresh_colors)

	return bgw
end

local function build_single_taskbar(s, theme, bar_height, fixed_width, spacing, menu_api)
	local container = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	local tag = s.selected_tag
	if not tag then
		return container
	end

	for _, c in ipairs(tag:clients() or {}) do
		if c.valid and not c.skip_taskbar and c.screen == s then
			local label = c.name or c.class or "App"
			container:add(build_group_tab(label, { c }, theme, bar_height, fixed_width, menu_api))
		end
	end

	return container
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(s, opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local theme = assert(opts.theme, "tabs.lua: opts.theme muss injiziert werden (Theme.tabs.get(...))")
	local menu_api = opts.menu_api
	local bar_height = require_number(opts.bar_height, "opts.bar_height")

	local width_factor = require_number(theme.width_factor, "theme.width_factor")
	local spacing = require_number(theme.spacing, "theme.spacing")

	local fixed_width = math.floor(width_factor * bar_height)

	-- ---------------------------------------------------------------------
	-- Widget
	-- ---------------------------------------------------------------------

	local box = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	local function refresh()
		box:reset()

		local built = build_single_taskbar(s, theme, bar_height, fixed_width, spacing, menu_api)
		for _, child in ipairs(built.children or {}) do
			box:add(child)
		end
	end

	local function delayed_refresh()
		gears.timer.delayed_call(refresh)
	end

	-- ---------------------------------------------------------------------
	-- Signals
	-- ---------------------------------------------------------------------

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
