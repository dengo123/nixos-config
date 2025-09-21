-- ~/.config/awesome/features/shell/menu/int.lua
local base = require("features.shell.menu.parts.base")
local defaults = require("features.shell.menu.widgets.apps")
local Popup = require("features.shell.menu.parts.popup")
local beautiful = require("beautiful")
local menubar = require("menubar")

local M = {}

local function merge_data(src)
	src = src or {}
	local pick = function(k)
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

	local api = base.build_popup({
		cfg = cfg,
		theme = cfg.menu_theme, -- <== roh durchreichen
		data = merge_data(cfg.menu_data),
		on_search = cfg.menu_on_search or function(_) end,
	})

	_G.__menu_api = {
		toggle = function(o)
			api:toggle(o)
		end,
		show = function(o)
			api:show(o)
		end,
		hide = function()
			api:hide()
		end,
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
	}

	awesome.connect_signal("menu::toggle", function()
		_G.__menu_api.toggle()
	end)
	awesome.connect_signal("menu::search_local", function()
		if api.focus_search then
			api:focus_search()
		end
	end)
	awesome.connect_signal("menu::search_web", function()
		if api.focus_search_web then
			api:focus_search_web()
		end
	end)

	local launcher = cfg.menu_launcher
		or Popup.make_launcher(api, (cfg.ui and cfg.ui.awesome_icon) or beautiful.awesome_icon, beautiful)

	return { menu = api, launcher = launcher }
end

return M
