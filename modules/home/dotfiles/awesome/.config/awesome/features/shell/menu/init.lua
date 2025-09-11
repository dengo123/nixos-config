-- ~/.config/awesome/features/shell/menu/init.lua
local core = require("features.shell.menu.core")
local defaults = require("features.shell.menu.data")
local menubar = require("menubar")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")

local M = {}

local function resolve_theme(theme)
	if type(theme) == "function" then
		theme = theme()
	end
	if type(theme) == "table" then
		return theme
	end
	local ok, tmod = pcall(require, "ui.theme")
	if ok and type(tmod.startmenu) == "function" then
		return tmod.startmenu()
	elseif ok and type(tmod) == "table" then
		return tmod
	end
	return {
		bg = "#222222",
		fg = "#ffffff",
		border_color = "#000000",
		border_width = 1,
		total_height = 500,
	}
end

local function merge_data(src)
	src = src or {}
	local function pick(k)
		return (src[k] ~= nil) and src[k] or defaults[k]
	end
	return {
		user = pick("user"),
		left_items = pick("left_items"),
		right_items = pick("right_items"),
		power_items = pick("power_items"),
	}
end

local function make_default_launcher(api, icon)
	local img = icon or beautiful.awesome_icon
	return wibox.widget({
		image = img,
		widget = wibox.widget.imagebox,
		buttons = gears.table.join(awful.button({}, 1, function()
			api:toggle()
		end)),
	})
end

-- Low-level: Popup/API erzeugen
function M.create(opts)
	opts = opts or {}
	local build_popup = (type(core) == "function") and core or core.build_popup
	assert(type(build_popup) == "function", "menu.core export mismatch: expected function 'build_popup'")
	return build_popup({
		theme = resolve_theme(opts.theme),
		data = merge_data(opts.data),
		cfg = opts.cfg,
	})
end

-- High-level: Menü an die App hängen (setzt cfg.mymainmenu / cfg.mylauncher)
function M.attach(cfg)
	cfg = cfg or {}
	if cfg.terminal then
		menubar.utils.terminal = cfg.terminal
	end

	local api = M.create({
		cfg = cfg,
		theme = cfg.menu_theme,
		data = cfg.menu_data,
	})

	-- Falls schon ein Start-Widget existiert, nicht überschreiben.
	-- Andernfalls einen einfachen Launcher bereitstellen.
	if not cfg.mylauncher then
		cfg.mylauncher = make_default_launcher(api, (cfg.ui and cfg.ui.awesome_icon) or beautiful.awesome_icon)
	end
	cfg.mymainmenu = api

	return api
end

return M
