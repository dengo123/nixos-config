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

local function entry_clients(entry)
	if type(entry) ~= "table" then
		return {}
	end

	return entry.clients or {}
end

local function entry_label(entry)
	if type(entry) ~= "table" then
		return "App"
	end

	return entry.label or "App"
end

-- =========================================================================
-- Tab Builder
-- =========================================================================

local function build_tab(entry, theme, bar_height, fixed_width, hooks)
	hooks = hooks or {}

	local clients = entry_clients(entry)
	local colors = theme.colors or {}

	local icon_size = tonumber(theme.icon_size)
	local pad_h = tonumber(theme.pad_h)
	local pad_v = tonumber(theme.pad_v)
	local radius = tonumber(theme.radius)
	local inactive_border_width = tonumber(theme.inactive_border_width)
	local title_len = tonumber(theme.title_len)
	local title_offset_y = tonumber(theme.title_offset_y)

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
		text = ellipsize(entry_label(entry), title_len),
	})

	local title = wibox.widget({
		{
			title_raw,
			top = math.max(0, title_offset_y),
			bottom = math.max(0, -title_offset_y),
			widget = wibox.container.margin,
		},
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
			if type(hooks.on_left_click) == "function" then
				hooks.on_left_click({
					widget = bgw,
					entry = entry,
					clients = clients,
					lead = (lead and lead.valid) and lead or pick_lead(clients),
				})
			end
		end),

		awful.button({}, 4, function()
			if type(hooks.on_scroll_up) == "function" then
				hooks.on_scroll_up({
					widget = bgw,
					entry = entry,
					clients = clients,
				})
			end
		end),

		awful.button({}, 5, function()
			if type(hooks.on_scroll_down) == "function" then
				hooks.on_scroll_down({
					widget = bgw,
					entry = entry,
					clients = clients,
				})
			end
		end)
	))

	bgw:connect_signal("button::press", function(_, lx, _, button)
		if button ~= 3 then
			return
		end

		if type(hooks.on_right_click) == "function" then
			local mc = mouse.coords()
			local x_left = mc.x - (lx or 0)

			hooks.on_right_click({
				widget = bgw,
				entry = entry,
				clients = clients,
				lead = (lead and lead.valid) and lead or pick_lead(clients),
				anchor = { x_left = x_left },
			})
		end
	end)

	client.connect_signal("focus", refresh_colors)
	client.connect_signal("unfocus", refresh_colors)

	return bgw
end

local function build_taskbar(entries, theme, bar_height, fixed_width, spacing, hooks)
	local container = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	for _, entry in ipairs(entries or {}) do
		container:add(build_tab(entry, theme, bar_height, fixed_width, hooks))
	end

	return container
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(_s, opts)
	opts = opts or {}

	local theme = opts.theme or {}
	local bar_height = tonumber(opts.bar_height)
	local entries_fn = opts.entries_fn

	local width_factor = tonumber(theme.width_factor)
	local spacing = tonumber(theme.spacing)

	local fixed_width = math.floor(width_factor * bar_height)

	local hooks = {
		on_left_click = opts.on_left_click,
		on_right_click = opts.on_right_click,
		on_scroll_up = opts.on_scroll_up,
		on_scroll_down = opts.on_scroll_down,
	}

	local box = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = spacing,
	})

	local function current_entries()
		if type(entries_fn) == "function" then
			local out = entries_fn()
			if type(out) == "table" then
				return out
			end
		end

		return {}
	end

	local function refresh()
		box:reset()

		local built = build_taskbar(current_entries(), theme, bar_height, fixed_width, spacing, hooks)
		for _, child in ipairs(built.children or {}) do
			box:add(child)
		end
	end

	local function delayed_refresh()
		gears.timer.delayed_call(refresh)
	end

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
		refresh = refresh,
	}
end

return M
