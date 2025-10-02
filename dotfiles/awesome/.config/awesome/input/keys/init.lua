-- ~/.config/awesome/input/keys/init.lua
local gears = require("gears")

-- Helpers einmalig laden und weiterreichen/weiterexportieren
local H = require("input.keys.helpers")

local M = {}

--- apply(cfg)
--  cfg = { modkey, terminal, launcher, browser, files, ... }
function M.apply(cfg)
	local modkey = cfg.modkey
	local join = gears.table.join

	-- globale Keysets aus den Teilmodulen zusammenführen
	local globalkeys = join(
		require("input.keys.global.navigation")(modkey),
		require("input.keys.global.tags")(modkey),
		require("input.keys.global.screens")(modkey),
		require("input.keys.global.layout")(modkey), -- enthält "promote to master"
		require("input.keys.global.state")(modkey),
		require("input.keys.global.apps")(modkey, cfg),
		require("input.keys.global.awesome")(modkey),
		require("input.keys.global.menu")(modkey),
		require("input.keys.global.power")(modkey, cfg.dialogs)
	)

	-- Export (optional für Zugriff in rc.lua/Rules o.ä.)
	M.globalkeys = globalkeys
	M.helpers = H

	-- aktivieren
	root.keys(globalkeys)
end

return M
