-- ~/.config/awesome/shell/notify/rules.lua
local naughty = require("naughty")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply()
	naughty.config.rules = naughty.config.rules or {}

	table.insert(naughty.config.rules, {
		rule = { app_name = "spotify-player" },
		properties = {
			icon = nil,
			image = nil,
			icon_size = 0,
		},
	})

	table.insert(naughty.config.rules, {
		rule = { app_name = "Spotify" },
		properties = {
			icon = nil,
			image = nil,
			icon_size = 0,
		},
	})
end

return M
