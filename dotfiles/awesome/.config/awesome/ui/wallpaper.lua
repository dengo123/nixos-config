-- ~/.config/awesome/ui/wallpaper.lua
local gears = require("gears")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

local M = {}

-- wählt je Screen Quelle (String/Funktion/Surface)
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

-- wählt je Screen Mode
local function get_mode(s, cfg)
	if cfg and type(cfg.per_screen) == "function" then
		local per = cfg.per_screen(s) or {}
		if per.mode then
			return per.mode
		end
	end
	return (cfg and cfg.mode) or "cover" -- cover = füllen (ohne Strecken), Standard
end

-- Anwenden je nach Mode
local function apply_mode(src, s, mode)
	if not src or src == "" then
		return
	end
	if mode == "stretch" then
		-- genau auf Screen strecken (ignoriert Seitenverhältnis)
		gears.wallpaper.maximized(src, s, true)
	elseif mode == "cover" or mode == "fill" then
		-- füllt den Screen, schneidet ggf. ab (bewahrt Seitenverhältnis)
		gears.wallpaper.maximized(src, s, false)
	elseif mode == "fit" then
		-- ganz einpassen (bewahrt Seitenverhältnis, lässt ggf. Ränder)
		if gears.wallpaper.fit then
			gears.wallpaper.fit(src, s, true) -- true = upscalen erlauben (falls vorhanden)
		else
			gears.wallpaper.maximized(src, s, false) -- fallback
		end
	elseif mode == "center" or mode == "centered" then
		if gears.wallpaper.centered then
			-- optional: BG-Farbe = beautiful.bg_normal oder nil
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
		-- Unbekannter Mode → cover
		gears.wallpaper.maximized(src, s, false)
	end
end

-- intern: auf einen Screen anwenden
local function apply(s, cfg)
	if not s then
		return
	end
	local src = get_source(s, cfg)
	local mode = get_mode(s, cfg)
	apply_mode(src, s, mode)
end

-- Hooks idempotent
function M.hook(cfg)
	-- Wir kapseln apply, damit cfg verfügbar bleibt
	local function handler(scr)
		apply(scr, cfg)
	end
	screen.disconnect_signal("property::geometry", handler)
	screen.connect_signal("property::geometry", handler)
end

-- Initialisierung
-- cfg.wallpaper: String/Funktion/Surface (global)
-- cfg.mode: "cover" | "stretch" | "fit" | "center" | "tile"
-- cfg.per_screen: function(s) -> { wallpaper=..., mode=... } (optional)
function M.init(cfg)
	cfg = cfg or {}

	-- Quelle der Wahrheit global setzen (optional; kann auch nur per_screen kommen)
	if cfg.wallpaper ~= nil then
		beautiful.wallpaper = cfg.wallpaper
	else
		local base = gfs.get_configuration_dir() .. "ui/wallpapers/"
		beautiful.wallpaper = function(_)
			return base .. "bliss2d.png"
		end
	end

	-- Hook + initial anwenden
	M.hook(cfg)
	for s in screen do
		apply(s, cfg)
	end
end

-- Laufzeitwechsel der Quelle (einmalig; pures convenience)
function M.set_source(src, cfg)
	beautiful.wallpaper = src
	for s in screen do
		apply(s, cfg or {})
	end
end

return M
