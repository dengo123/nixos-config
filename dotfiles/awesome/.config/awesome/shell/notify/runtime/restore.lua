-- ~/.config/awesome/shell/notify/runtime/restore.lua
local gears = require("gears")

local M = {}

local runtime = {
	history = nil,
}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.history = args.history
	return M
end

function M.run(data)
	local History = runtime.history

	if type(data) ~= "table" then
		return false
	end

	if not (History and type(History.replace_all) == "function") then
		return false
	end

	History.replace_all(data.entries or {})
	return true
end

function M.restore_on_start(fn)
	gears.timer.start_new(0.10, function()
		if type(fn) == "function" then
			fn()
		end
		return false
	end)
end

return M
