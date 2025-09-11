-- ~/.config/awesome/input/keys/init.lua
local gears = require("gears")

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
		require("input.keys.global.prompt")(modkey),
		require("input.keys.global.awesome")(modkey)
	)

	-- Export optional
	M.globalkeys = globalkeys

	root.keys(globalkeys)
end

return M
