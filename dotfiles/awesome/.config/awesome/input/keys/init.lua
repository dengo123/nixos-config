-- ~/.config/awesome/input/keys/init.lua
local gears = require("gears")
local Escape = require("input.keys.runtime.escape")
local M = {}

function M.apply(cfg)
	local modkey = cfg.input.modkey
	local join = gears.table.join

	local globalkeys = join(
		require("input.keys.global.navigation")(modkey),
		require("input.keys.global.tags")(modkey, cfg.actions.workspaces.tags),
		require("input.keys.global.screens")(modkey, cfg.actions.windowing.screens),
		require("input.keys.global.layout")(modkey),
		require("input.keys.global.state")(modkey),
		require("input.keys.global.apps")(modkey, cfg),
		require("input.keys.global.awesome")(modkey),
		require("input.keys.global.run")(modkey, cfg.api and cfg.api.launchers),
		require("input.keys.global.power")(modkey, cfg.api and cfg.api.launchers),
		require("input.keys.global.logoff")(modkey, cfg.api and cfg.api.launchers),
		require("input.keys.global.screenshot")(modkey),
		require("input.keys.global.media")(modkey, cfg)
	)

	M.globalkeys = globalkeys
	root.keys(globalkeys)

	Escape.init({
		globalkeys = globalkeys,
		overlays = cfg.overlays,
	})
end

return M
