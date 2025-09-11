-- ~/.config/awesome/input/init.lua
local keys = require("input.keys")
local mouse = require("input.mouse")

local M = {
	keys = keys,
	mouse = mouse,
}

--- Apply all input bindings (keys + root mouse)
-- @param cfg table: { modkey, terminal, launcher, browser, files, mymainmenu }
function M.apply(cfg)
	keys.apply(cfg)
	mouse.apply_root(cfg.mymainmenu)
	return M
end

return M
