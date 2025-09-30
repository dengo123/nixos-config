-- ~/.config/awesome//shell/menu/init.lua
local container = require("shell.menu.container")
local Lib = require("shell.menu.lib")
local defaults = Lib.defaults()
local beautiful = require("beautiful")
local menubar = require("menubar")

local M = {}

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

function M.setup(cfg)
	cfg = cfg or {}
	if cfg.terminal then
		menubar.utils.terminal = cfg.terminal
	end

	-- Menü aufbauen
	local api = container.build_popup({
		cfg = cfg,
		theme = cfg.menu_theme, -- roh durchreichen
		data = merge_data(cfg.menu_data),
		on_search = cfg.menu_on_search or function(_) end,
	})

	-- >>> Globale Menü-API bereitstellen
	_G.__menu_api = api

	-- Komfort-Signale
	awesome.connect_signal("menu::toggle", function()
		_G.__menu_api:toggle()
	end)
	awesome.connect_signal("menu::search_local", function()
		if _G.__menu_api.focus_search then
			_G.__menu_api:focus_search()
		end
	end)
	awesome.connect_signal("menu::search_web", function()
		if _G.__menu_api.focus_search_web then
			_G.__menu_api:focus_search_web()
		end
	end)

	-- Launcher (Icon-Button) – via API (Option B)
	local launcher = cfg.menu_launcher
		or api:make_launcher((cfg.ui and cfg.ui.awesome_icon) or beautiful.awesome_icon, beautiful)

	return { menu = api, launcher = launcher }
end

return M
