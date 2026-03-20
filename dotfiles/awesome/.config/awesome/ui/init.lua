-- ~/.config/awesome/ui/init.lua
local Colors = require("ui.colors")
local Helpers = require("ui.helpers")

local M = {
	wallpaper = require("ui.wallpaper"),
}

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

	-- ---------------------------------------------------------------------
	-- Wallpaper
	-- ---------------------------------------------------------------------

	if M.wallpaper and type(M.wallpaper.init) == "function" then
		M.wallpaper.init(cfg)
	end

	return M
end

return M
