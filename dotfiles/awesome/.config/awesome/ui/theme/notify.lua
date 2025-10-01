-- ~/.config/awesome/features/shell/notify/init.lua
local naughty = require("naughty")
local gears = require("gears")

local M = {}

function M.init(opts)
	opts = opts or {}

	local BG = opts.bg or "#F5E6B3" -- beige
	local FG = opts.fg or "#000000" -- schwarz
	local BORDER = opts.border or "#C8B27A" -- Randfarbe
	local RADIUS = opts.radius or 8
	local TIMEOUT = opts.timeout or 3
	local ICON_SIZE = opts.icon_size or 24
	local POS = opts.position or "bottom_right"
	local SPEECH = opts.speech or true

	-- Form (runde Ecken oder „Sprechblase“)
	-- Zipfel NACH UNTEN (unten rechts)
	local function speech_bubble(cr, w, h)
		local r = RADIUS
		local tail_w, tail_h = 12, 8

		-- Körper: in der Höhe kürzen, damit der Zipfel unten "anflanscht"
		local body_h = h - tail_h
		gears.shape.rounded_rect(cr, w, body_h, r)

		-- Zipfel-Dreieck an der Unterkante, nahe der rechten Ecke
		cr:new_sub_path()
		cr:move_to(w - r - tail_w, body_h) -- Start: unten, etwas links der Ecke
		cr:line_to(w - r - tail_w / 2, body_h + tail_h) -- Spitze: ragt nach unten
		cr:line_to(w - r, body_h) -- Ende: direkt an der Rundung
		cr:close_path()
	end

	local shape_fn = SPEECH and speech_bubble or function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, RADIUS)
	end

	-- Globale Defaults (wir brauchen keinen request::display-Handler)
	naughty.config.defaults.position = POS
	naughty.config.defaults.timeout = TIMEOUT
	naughty.config.defaults.margin = 10
	naughty.config.defaults.border_width = 1
	naughty.config.defaults.border_color = BORDER
	naughty.config.defaults.bg = BG
	naughty.config.defaults.fg = FG
	naughty.config.defaults.shape = shape_fn
	naughty.config.defaults.icon_size = ICON_SIZE

	-- Presets anpassen (falls irgendwer die nutzt)
	for _, p in pairs({ "low", "normal", "critical" }) do
		local preset = naughty.config.presets[p]
		preset.bg = BG
		preset.fg = FG
		preset.border_width = 1
		preset.border_color = BORDER
		preset.shape = shape_fn
	end

	-- Regeln: Spotify-Icons/Cover entfernen (falls eingebettet)
	naughty.config.rules = naughty.config.rules or {}

	table.insert(naughty.config.rules, {
		rule = { app_name = "spotify-player" },
		properties = { icon = nil, image = nil, icon_size = 0 },
	})

	table.insert(naughty.config.rules, {
		rule = { app_name = "Spotify" },
		properties = { icon = nil, image = nil, icon_size = 0 },
	})
end

return M
