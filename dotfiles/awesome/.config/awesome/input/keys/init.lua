-- ~/.config/awesome/input/keys/init.lua
local gears = require("gears")
local H = require("input.keys.helpers")
local M = {}

function M.apply(cfg)
	local modkey = cfg.modkey
	local join = gears.table.join

	local globalkeys = join(
		require("input.keys.global.navigation")(modkey),
		require("input.keys.global.tags")(modkey),
		require("input.keys.global.screens")(modkey),
		require("input.keys.global.layout")(modkey),
		require("input.keys.global.state")(modkey),
		require("input.keys.global.apps")(modkey, cfg),
		require("input.keys.global.awesome")(modkey),
		require("input.keys.global.run")(modkey, cfg.launchers),
		require("input.keys.global.power")(modkey, cfg.launchers),
		require("input.keys.global.screenshot")(modkey),
		require("input.keys.global.media")(modkey, cfg)
	)

	M.globalkeys = globalkeys
	M.helpers = H
	root.keys(globalkeys)
end

return M
