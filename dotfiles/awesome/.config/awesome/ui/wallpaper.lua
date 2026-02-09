-- ~/.config/awesome/ui/wallpaper.lua
local gears = require("gears")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

local M = {}

-- ========= helpers =========

local function get_source(s, cfg)
	if cfg and type(cfg.per_screen) == "function" then
		local per = cfg.per_screen(s) or {}
		if per.wallpaper ~= nil then
			return per.wallpaper
		end
	end

	if beautiful.wallpaper then
		return (type(beautiful.wallpaper) == "function") and beautiful.wallpaper(s) or beautiful.wallpaper
	end

	local base = gfs.get_configuration_dir() .. "ui/wallpapers/"
	return base .. "bliss2d.png"
end

local function get_mode(s, cfg)
	if cfg and type(cfg.per_screen) == "function" then
		local per = cfg.per_screen(s) or {}
		if per.mode then
			return per.mode
		end
	end
	return (cfg and cfg.mode) or "cover"
end

local function apply_mode(src, s, mode)
	if not src or src == "" then
		return
	end

	if mode == "stretch" then
		gears.wallpaper.maximized(src, s, true)
	elseif mode == "cover" or mode == "fill" then
		gears.wallpaper.maximized(src, s, false)
	elseif mode == "fit" then
		if gears.wallpaper.fit then
			gears.wallpaper.fit(src, s, true)
		else
			gears.wallpaper.maximized(src, s, false)
		end
	elseif mode == "center" or mode == "centered" then
		if gears.wallpaper.centered then
			gears.wallpaper.centered(src, s, beautiful.bg_normal, true)
		else
			gears.wallpaper.maximized(src, s, false)
		end
	elseif mode == "tile" or mode == "tiled" then
		if gears.wallpaper.tiled then
			gears.wallpaper.tiled(src, s)
		else
			gears.wallpaper.maximized(src, s, false)
		end
	else
		gears.wallpaper.maximized(src, s, false)
	end
end

local function apply(scr, cfg)
	if not scr then
		return
	end
	local src = get_source(scr, cfg)
	local mode = get_mode(scr, cfg)
	apply_mode(src, scr, mode)
end

-- ========= debounce refresh =========

M._cfg = {}
M._handler = nil

M._debounce = gears.timer({
	timeout = 0.12,
	autostart = false,
	single_shot = true,
	callback = function()
		-- 1) kurz nachdem alles “meistens” steht
		M.refresh()

		-- 2) nochmal später (HDMI/4K zieht gern nach)
		gears.timer.start_new(0.35, function()
			M.refresh()
			return false
		end)
	end,
})

function M.refresh()
	for scr in screen do
		apply(scr, M._cfg or {})
	end
end

function M.hook(cfg)
	M._cfg = cfg or {}

	-- alten Handler sauber entfernen
	if M._handler then
		screen.disconnect_signal("property::geometry", M._handler)
	end

	M._handler = function(_)
		if M._debounce.started then
			M._debounce:stop()
		end
		M._debounce:start()
	end

	screen.connect_signal("property::geometry", M._handler)
end

function M.init(cfg)
	cfg = cfg or {}

	if cfg.wallpaper ~= nil then
		beautiful.wallpaper = cfg.wallpaper
	else
		local base = gfs.get_configuration_dir() .. "ui/wallpapers/"
		beautiful.wallpaper = function(_)
			return base .. "bliss2d.png"
		end
	end

	M.hook(cfg)
	M.refresh()

	-- ===== External triggers =====
	awesome.connect_signal("ui::wallpaper_refresh", function()
		if M._debounce.started then
			M._debounce:stop()
		end
		M._debounce:start()
	end)

	awesome.connect_signal("autorandr::applied", function()
		if M._debounce.started then
			M._debounce:stop()
		end
		M._debounce:start()
	end)
end

function M.set_source(src)
	beautiful.wallpaper = src
	M.refresh()
end

return M
