-- ui/init.lua
local Colors = require("ui.colors")
local Helpers = require("ui.helpers")

local M = {
	theme = {},
	wallpaper = require("ui.wallpaper"),
}

local PARTS = { "start", "menu", "notify", "tabs", "windows", "wibar", "power" }

for _, name in ipairs(PARTS) do
	local ok, mod = pcall(require, "ui.theme." .. name)
	if ok and type(mod) == "table" then
		M.theme[name] = mod
	end
end

function M.init(cfg)
	cfg = cfg or {}
	cfg.colors = cfg.colors or Colors.get()
	cfg.helpers = cfg.helpers or Helpers

	-- 1) Theme-Parts initialisieren (setzen beautiful.*)
	for _, mod in pairs(M.theme) do
		if type(mod.init) == "function" then
			pcall(mod.init, cfg)
		end
	end

	-- 2) Wallpaper (falls es auch beautiful nutzt)
	if M.wallpaper and type(M.wallpaper.init) == "function" then
		M.wallpaper.init(cfg)
	end

	-- 3) ZENTRAL: Tabellen einfrieren + Keys sperren
	local beautiful = require("beautiful")

	-- Tabellen deep-freezen (nur wenn vorhanden)
	if beautiful.tags_indicator then
		beautiful.tags_indicator = Helpers.freeze_table(beautiful.tags_indicator, "error")
	end
	-- (ergänze weitere Theme-Tabellen hier bei Bedarf)
	-- if beautiful.tabs_style then beautiful.tabs_style = Helpers.freeze_table(beautiful.tabs_style, "error") end
	-- if beautiful.menu_style then beautiful.menu_style = Helpers.freeze_table(beautiful.menu_style, "error") end

	-- Präfixe sperren (alle relevanten Bereiche zentral dicht machen)
	Helpers.lock_beautiful_by_prefix({
		"wibar_",
		"systray_",
		"clock_",
		"layoutbox_", -- Bar
		"border_",
		"titlebar_", -- Windows
		"menu_",
		"notify_",
		"tabs_",
		"start_",
		"power_", -- weitere Theme-Module
		-- füge hier weitere Präfixe hinzu, falls du später neue Bereiche einführst
	}, "error")

	return M
end

return M
