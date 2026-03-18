-- ~/.config/awesome/ui/init.lua
local Colors = require("ui.colors")
local Helpers = require("ui.helpers")

local M = {
	theme = {},
	wallpaper = require("ui.wallpaper"),
}

local THEME_PARTS = {
	"start",
	"menu",
	"notify",
	"tabs",
	"windows",
	"wibar",
	"power",
	"run",
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function load_theme_parts()
	local out = {}

	for _, name in ipairs(THEME_PARTS) do
		local ok, mod = pcall(require, "ui.theme." .. name)

		if ok and type(mod) == "table" then
			out[name] = mod
		end
	end

	return out
end

local function init_theme_parts(cfg, theme_parts)
	for _, name in ipairs(THEME_PARTS) do
		local mod = theme_parts[name]

		if mod and type(mod.init) == "function" then
			pcall(mod.init, cfg)
		end
	end
end

local function lock_theme_tables(beautiful)
	if beautiful.tags_indicator then
		beautiful.tags_indicator = Helpers.freeze_table(beautiful.tags_indicator, "error")
	end
end

local function lock_theme_prefixes()
	Helpers.lock_beautiful_by_prefix({
		"wibar_",
		"systray_",
		"clock_",
		"layoutbox_",
		"border_",
		"titlebar_",
		"menu_",
		"hotkeys_",
		"notify_",
		"tabs_",
		"start_",
		"power_",
		"run_",
	}, "error")
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	-- ---------------------------------------------------------------------
	-- Context
	-- ---------------------------------------------------------------------

	cfg.colors = cfg.colors or Colors.get()
	cfg.helpers = cfg.helpers or Helpers

	M.theme = load_theme_parts()

	-- ---------------------------------------------------------------------
	-- Theme
	-- ---------------------------------------------------------------------

	init_theme_parts(cfg, M.theme)

	-- ---------------------------------------------------------------------
	-- Wallpaper
	-- ---------------------------------------------------------------------

	if M.wallpaper and type(M.wallpaper.init) == "function" then
		M.wallpaper.init(cfg)
	end

	-- ---------------------------------------------------------------------
	-- Locking
	-- ---------------------------------------------------------------------

	local beautiful = require("beautiful")

	lock_theme_tables(beautiful)
	lock_theme_prefixes()

	return M
end

return M
