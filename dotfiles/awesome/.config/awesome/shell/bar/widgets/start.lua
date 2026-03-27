-- ~/.config/awesome/shell/bar/widgets/start.lua
local awful = require("awful")
local gears = require("gears")
local gfs = require("gears.filesystem")
local gsurface = require("gears.surface")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function needs_shell(cmd)
	if type(cmd) ~= "string" then
		return false
	end

	return cmd:find("[%s][\"']")
		or cmd:find("||", 1, true)
		or cmd:find("&&", 1, true)
		or cmd:find("~", 1, true)
		or cmd:find("%$")
		or cmd:find("[<>|;&]")
end

local function spawn_cmd(cmd)
	if type(cmd) == "table" then
		awful.spawn(cmd)
		return true
	end

	if type(cmd) == "string" and cmd ~= "" then
		if needs_shell(cmd) then
			awful.spawn.with_shell(cmd)
		else
			awful.spawn(cmd)
		end
		return true
	end

	return false
end

local function resolve_ctx(opts)
	return (opts and opts.ctx) or {}
end

local function resolve_cfg(opts)
	local c = resolve_ctx(opts)
	return (opts and opts.cfg) or c.cfg or {}
end

local function resolve_menu_api(opts)
	local c = resolve_ctx(opts)

	return (opts and opts.menu_api)
		or (c.features and c.features.menu)
		or (c.shell and c.shell.menu)
		or (c.api and c.api.menu)
		or nil
end

local function resolve_screen(opts)
	return (opts and opts.screen) or (mouse and mouse.screen) or nil
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local screen = resolve_screen(opts)
	local theme = opts.theme or {}
	local bar_height = tonumber(opts.bar_height)
	local cfg = resolve_cfg(opts)
	local menu_api = resolve_menu_api(opts)

	local apps_cfg = cfg.apps or {}
	local bar_cfg = cfg.bar or {}
	local start_action = tostring(opts.start_action or bar_cfg.start_action or "menu"):lower()

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	local bg = theme.bg
	local bg_hover = theme.bg_hover
	local fg = theme.fg
	local shape = theme.shape
	local margin = theme.margin or { left = 0, right = 0, top = 0, bottom = 0 }

	local width = math.floor((tonumber(theme.width_factor) or 4) * (bar_height or 1))

	-- ---------------------------------------------------------------------
	-- Icon
	-- ---------------------------------------------------------------------

	local icon_path = theme.icon
	if type(icon_path) == "string" and not icon_path:match("^/") then
		icon_path = gfs.get_configuration_dir() .. icon_path
	end

	local icon_surface = (type(icon_path) == "string") and gsurface.load_uncached(icon_path) or icon_path

	local inner_height = (bar_height or 0) - ((margin.top or 0) + (margin.bottom or 0))
	local icon_px = math.min(theme.icon_size or inner_height, inner_height)

	local icon_widget = wibox.widget({
		image = icon_surface,
		resize = true,
		upscale = true,
		downscale = true,
		forced_width = icon_px,
		forced_height = icon_px,
		widget = wibox.widget.imagebox,
	})

	-- ---------------------------------------------------------------------
	-- Label
	-- ---------------------------------------------------------------------

	local label_widget = wibox.widget({
		text = theme.label,
		font = theme.font,
		widget = wibox.widget.textbox,
	})

	-- ---------------------------------------------------------------------
	-- Layout
	-- ---------------------------------------------------------------------

	local row_inner = wibox.widget({
		icon_widget,
		label_widget,
		spacing = tonumber(theme.spacing),
		layout = wibox.layout.fixed.horizontal,
	})

	local row = wibox.widget({
		{
			row_inner,
			left = margin.left or 0,
			right = margin.right or 0,
			top = margin.top or 0,
			bottom = margin.bottom or 0,
			widget = wibox.container.margin,
		},
		fg = fg,
		widget = wibox.container.background,
	})

	local btn = wibox.widget({
		row,
		bg = bg,
		shape = shape,
		forced_width = width,
		forced_height = theme.fixed_height and bar_height or nil,
		widget = wibox.container.background,
	})

	-- ---------------------------------------------------------------------
	-- Helpers
	-- ---------------------------------------------------------------------

	local function show_menu()
		if menu_api and type(menu_api.show_for_start_widget) == "function" then
			menu_api.show_for_start_widget(screen, btn)
			return true
		end

		return false
	end

	local function hide_menu()
		if menu_api and type(menu_api.hide) == "function" then
			menu_api.hide()
			return true
		end

		return false
	end

	local function toggle_menu()
		if menu_api and type(menu_api.is_open) == "function" and menu_api.is_open() then
			return hide_menu()
		end

		return show_menu()
	end

	local function on_left_click()
		if start_action == "menu" then
			toggle_menu()
			return
		end

		if start_action == "rofi" then
			awful.spawn.with_shell("rofi -show drun")
			return
		end

		if start_action == "editor" then
			if spawn_cmd(apps_cfg.editor) then
				return
			end
			toggle_menu()
			return
		end

		if start_action == "terminal" then
			if spawn_cmd(apps_cfg.terminal) then
				return
			end
			toggle_menu()
			return
		end

		toggle_menu()
	end

	-- ---------------------------------------------------------------------
	-- Mouse
	-- ---------------------------------------------------------------------

	btn:connect_signal("mouse::enter", function()
		btn.bg = bg_hover
	end)

	btn:connect_signal("mouse::leave", function()
		btn.bg = bg
	end)

	btn:buttons(gears.table.join(
		awful.button({}, 1, on_left_click),
		awful.button({}, 3, function()
			toggle_menu()
		end)
	))

	return btn
end

return M
