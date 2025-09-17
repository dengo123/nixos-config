-- features/shell/menu/init.lua
local base = require("features.shell.menu.parts.base")
local defaults = require("features.shell.menu.widgets.apps")
local Popup = require("features.shell.menu.parts.popup") -- für make_launcher
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
	return {
		bg = "#222222",
		fg = "#ffffff",
		border_color = "#000000",
		border_width = 1,
		total_height = 500,
	}
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

-- Menü-Setup: baut Popup + Launcher
-- Rückgabe: { menu = api(:show/:hide/:toggle/:focus_search), launcher = widget }
function M.setup(cfg)
	cfg = cfg or {}
	if cfg.terminal then
		menubar.utils.terminal = cfg.terminal
	end

	local api = base.build_popup({
		cfg = cfg,
		theme = resolve_theme(cfg.menu_theme),
		data = merge_data(cfg.menu_data),
		on_search = cfg.menu_on_search or function(_) end,
	})

	local launcher = cfg.menu_launcher
		or Popup.make_launcher(api, (cfg.ui and cfg.ui.awesome_icon) or beautiful.awesome_icon, beautiful)

	return { menu = api, launcher = launcher }
end

return M
