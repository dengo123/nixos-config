-- ~/.config/awesome/ui/wallpaper.lua
local gears = require("gears")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local naughty_ok, naughty = pcall(require, "naughty")

local M = {}

local function log(msg)
	if naughty_ok then
		naughty.notify({ title = "Wallpaper", text = tostring(msg) })
	end
	print("[Wallpaper] " .. tostring(msg))
end

local function apply(s)
	if not s then
		return
	end
	local wp = beautiful.wallpaper
	if not wp then
		log("beautiful.wallpaper=nil")
		return
	end
	if type(wp) == "function" then
		local ok, val = pcall(wp, s)
		if not ok then
			log("wallpaper fn error: " .. tostring(val))
			return
		end
		wp = val
	end
	if not wp or wp == "" then
		log("wallpaper leer")
		return
	end
	log(("apply screen %s -> %s"):format(s.index or "?", tostring(wp)))
	gears.wallpaper.maximized(wp, s, true)
end

function M.set_source(src)
	beautiful.wallpaper = src
	for sc in screen do
		apply(sc)
	end
end

function M.hook()
	screen.disconnect_signal("property::geometry", apply)
	screen.connect_signal("property::geometry", apply)
	log("hooked property::geometry")
end

function M.apply_all()
	for s in screen do
		apply(s)
	end
end

function M.init(cfg)
	cfg = cfg or {}

	if cfg.wallpaper ~= nil then
		beautiful.wallpaper = cfg.wallpaper -- String oder function(s)->string
		log("source set from cfg")
	else
		-- >>> HIER: Basis-Pfad auf ui/wallpapers setzen
		local base = gfs.get_configuration_dir() .. "ui/wallpapers/"
		local path = base .. "bliss2d.png"
		beautiful.wallpaper = function(_)
			return path
		end
		log("source set to " .. path)
	end

	M.hook()
	M.apply_all()
end

return M
