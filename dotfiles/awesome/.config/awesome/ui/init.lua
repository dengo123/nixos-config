-- ui/init.lua
local Colors = require("ui.colors")
local Helpers = require("ui.helpers") -- 👈 NEU

local M = {
	theme = {}, -- will be filled with parts directly
	wallpaper = require("ui.wallpaper"),
}

-- list your theme parts here (add/remove freely)
local PARTS = {
	"start",
	"menu",
	"notify",
	"tabs",
	"windows",
	"wibar",
	"power",
}

-- load parts directly: ui/theme/<part>.lua
for _, name in ipairs(PARTS) do
	local ok, mod = pcall(require, "ui.theme." .. name)
	if ok and type(mod) == "table" then
		M.theme[name] = mod
	end
end

function M.init(cfg)
	cfg = cfg or {}

	-- Palette & Helpers global an alle Module durchreichen
	cfg.colors = cfg.colors or Colors.get()
	cfg.helpers = cfg.helpers or Helpers -- 👈 NEU

	for _, mod in pairs(M.theme) do
		if type(mod.init) == "function" then
			pcall(mod.init, cfg)
		end
	end

	if M.wallpaper and type(M.wallpaper.init) == "function" then
		M.wallpaper.init(cfg)
	end
	return M
end

return M
