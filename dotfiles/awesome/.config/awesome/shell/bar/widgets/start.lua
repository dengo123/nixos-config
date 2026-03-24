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

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(opts)
	opts = opts or {}

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local screen = opts.screen or (mouse and mouse.screen) or nil
	local theme = opts.theme
	local bar_height = tonumber(opts.bar_height)
	local cfg = opts.cfg or {}
	local menu_api = opts.menu_api

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
	local margin = theme.margin

	local width = math.floor(tonumber(theme.width_factor) * bar_height)

	-- ---------------------------------------------------------------------
	-- Icon
	-- ---------------------------------------------------------------------

	local icon_path = theme.icon
	if type(icon_path) == "string" and not icon_path:match("^/") then
		icon_path = gfs.get_configuration_dir() .. icon_path
	end

	local icon_surface = (type(icon_path) == "string") and gsurface.load_uncached(icon_path) or icon_path

	local inner_height = bar_height - (margin.top + margin.bottom)
	local icon_px = math.min(theme.icon_size, inner_height)

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
			left = margin.left,
			right = margin.right,
			top = margin.top,
			bottom = margin.bottom,
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
