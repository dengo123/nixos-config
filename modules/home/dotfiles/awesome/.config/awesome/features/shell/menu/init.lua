-- features/shell/menu/init.lua
local core = require("features.shell.menu.core")
local defaults = require("features.shell.menu.shared.data")
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local menubar = require("menubar")

local M = {}

-- Theme auflösen (Fn | Table | ui.theme Fallback)
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
	return { bg = "#222222", fg = "#ffffff", border_color = "#000000", border_width = 1, total_height = 500 }
end

-- Daten mergen mit Defaults
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

-- Launcher-Widget (klick -> api:toggle())
local function make_launcher(api, icon)
	return wibox.widget({
		image = icon or beautiful.awesome_icon,
		widget = wibox.widget.imagebox,
		buttons = gears.table.join(awful.button({}, 1, function()
			api:toggle()
		end)),
	})
end

-- Fallback-Suche: öffnet Browser mit DuckDuckGo
local function default_on_search(q)
	if not q or #q == 0 then
		return
	end
	local url_q = q:gsub("%s+", "+")
	awful.spawn.with_shell('xdg-open "https://duckduckgo.com/?q=' .. url_q .. '"')
end

-- Menü-Setup: baut Popup + Launcher
-- Rückgabe: { menu = api(:show/:hide/:toggle/:focus_search), launcher = widget }
function M.setup(cfg)
	cfg = cfg or {}
	if cfg.terminal then
		menubar.utils.terminal = cfg.terminal
	end

	local api = core.build_popup({
		cfg = cfg,
		theme = resolve_theme(cfg.menu_theme),
		data = merge_data(cfg.menu_data),
		on_search = cfg.menu_on_search or default_on_search,
	})

	local launcher = cfg.menu_launcher or make_launcher(api, (cfg.ui and cfg.ui.awesome_icon) or beautiful.awesome_icon)

	return { menu = api, launcher = launcher }
end

return M
