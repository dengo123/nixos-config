-- ~/.config/awesome/ui/theme/notify.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi
local naughty = require("naughty")
local gears = require("gears")

local M = {}

function M.init(cfg)
	cfg = cfg or {}
	local C = cfg.colors or {}
	local H = cfg.helpers or {}

	-- sanft dunklere Creme als Border (ca. -14%), Fallback: Schwarz
	local border = (H and H.adjust_color) and H.adjust_color(C.creme, -14) or (C.black or "#000000")

	-- Quelle der Wahrheit (Theme)
	beautiful.notify = {
		-- Farben
		bg = C.creme,
		fg = C.black,
		border = border,

		-- Layout / Stil
		radius = dpi(8),
		icon_size = dpi(24),
		margin = dpi(10),
		border_w = dpi(1),

		-- Verhalten
		timeout = 3,
		position = "bottom_right",
		speech = true, -- Sprechblase (Zipfel unten rechts)
	}

	-- Shape: Sprechblase (unten rechts) oder runde Ecken
	local function speech_bubble(cr, w, h)
		local r = beautiful.notify.radius or dpi(8)
		local tail_w, tail_h = 12, 8
		local body_h = h - tail_h
		gears.shape.rounded_rect(cr, w, body_h, r)
		cr:new_sub_path()
		cr:move_to(w - r - tail_w, body_h)
		cr:line_to(w - r - tail_w / 2, body_h + tail_h)
		cr:line_to(w - r, body_h)
		cr:close_path()
	end
	local shape_fn = (beautiful.notify.speech and speech_bubble)
		or function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, beautiful.notify.radius or dpi(8))
		end

	-- Defaults aus Theme nach naughty übernehmen
	local N = beautiful.notify
	naughty.config.defaults.position = N.position
	naughty.config.defaults.timeout = N.timeout
	naughty.config.defaults.margin = N.margin
	naughty.config.defaults.border_width = N.border_w
	naughty.config.defaults.border_color = N.border
	naughty.config.defaults.bg = N.bg
	naughty.config.defaults.fg = N.fg
	naughty.config.defaults.shape = shape_fn
	naughty.config.defaults.icon_size = N.icon_size

	-- ZENTRIERTER Inhalt via widget_template
	naughty.config.defaults.widget_template = {
		{
			{
				{
					id = "title_role",
					align = "center",
					valign = "center",
					wrap = "word_char",
					widget = wibox.widget.textbox,
				},
				{
					id = "text_role",
					align = "center",
					valign = "center",
					wrap = "word_char", -- schöne Umbrüche
					widget = wibox.widget.textbox,
				},
				spacing = dpi(4),
				layout = wibox.layout.fixed.vertical,
			},
			margins = N.margin or dpi(10),
			widget = wibox.container.margin,
		},
		bg = N.bg,
		shape = shape_fn,
		widget = wibox.container.background,
	}

	-- Presets angleichen
	for _, p in pairs({ "low", "normal", "critical" }) do
		local preset = naughty.config.presets[p]
		preset.bg = N.bg
		preset.fg = N.fg
		preset.border_width = N.border_w
		preset.border_color = N.border
		preset.shape = shape_fn
		preset.timeout = N.timeout
		preset.icon_size = N.icon_size
	end

	-- Regeln: Spotify-Icons/Cover entfernen
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
