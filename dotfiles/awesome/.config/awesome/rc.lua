-- ~/.config/awesome/rc.lua
pcall(require, "luarocks.loader")

local gears = require("gears")
local confdir = gears.filesystem.get_configuration_dir()
package.path = confdir .. "?.lua;" .. confdir .. "?/init.lua;" .. package.path

-- Ein einziger Einstiegspunkt
require("system").init()
