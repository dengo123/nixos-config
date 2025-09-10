local gears, beautiful = require("gears"), require("beautiful")
local M = {}
function M.apply(cfg)
	local path = cfg.theme
	if not path:match("^/") then
		path = gears.filesystem.get_themes_dir() .. path
	end
	beautiful.init(path)
end

return M
