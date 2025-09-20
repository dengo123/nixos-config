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

	-- <<< NEU: globale Menü-API für Keybinds bereitstellen >>>
	_G.__menu_api = {
		toggle = function(opts)
			api:toggle(opts)
		end, -- Super+Space
		show = function(opts)
			api:show(opts)
		end,
		hide = function()
			api:hide()
		end,

		-- falls dein base.build_popup diese Funktionen anbietet:
		focus_search = function()
			if api.focus_search then
				api:focus_search()
			end
		end,
		focus_search_web = function()
			if api.focus_search_web then
				api:focus_search_web()
			end
		end,

		-- optional: Signals als Fallback, wenn du auf _G verzichten willst
		-- (Keybinds können awesome.emit_signal('menu::toggle') senden)
	}
	awesome.connect_signal("menu::toggle", function()
		_G.__menu_api.toggle()
	end)
	awesome.connect_signal("menu::search_local", function()
		if _G.__menu_api.focus_search then
			_G.__menu_api.focus_search()
		end
	end)
	awesome.connect_signal("menu::search_web", function()
		if _G.__menu_api.focus_search_web then
			_G.__menu_api.focus_search_web()
		end
	end)
	-- <<< NEU Ende >>>

	local launcher = cfg.menu_launcher
		or Popup.make_launcher(api, (cfg.ui and cfg.ui.awesome_icon) or beautiful.awesome_icon, beautiful)

	return { menu = api, launcher = launcher }
end

return M
