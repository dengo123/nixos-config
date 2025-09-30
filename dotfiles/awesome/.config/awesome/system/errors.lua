-- ~/.config/awesome/system/errors.lua
local naughty = require("naughty")

local M = {}

function M.hook()
	-- Startup errors
	if awesome.startup_errors then
		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Startup errors",
			text = awesome.startup_errors,
		})
	end

	-- Runtime errors
	local in_error = false
	awesome.connect_signal("debug::error", function(err)
		if in_error then
			return
		end
		in_error = true
		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Error",
			text = tostring(err),
		})
		in_error = false
	end)
end

return M
