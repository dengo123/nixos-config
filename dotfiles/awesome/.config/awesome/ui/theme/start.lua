-- ui/theme/start.lua
local beautiful = require("beautiful")
local gears = require("gears")
local gfs = require("gears.filesystem")

local S = {}

function S.init(_) end

function S.get(overrides)
	overrides = overrides or {}

	local DEFAULT_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"
	local H = tonumber(beautiful.wibar_height) or 28

	local function right_rounded(cr, w, h)
		local r = overrides.radius or (beautiful.border_radius or 32)
		if gears.shape.partially_rounded_rect then
			gears.shape.partially_rounded_rect(cr, w, h, false, true, true, false, r)
		else
			gears.shape.rounded_rect(cr, w, h, r)
		end
	end

	return {
		-- Inhalt
		label = (overrides.label or "start"):lower(), -- klein
		icon = overrides.icon or DEFAULT_ICON,

		-- Größen & Abstände
		icon_size = overrides.icon_size or math.floor(H * 0.9), -- größer (90% der Wibar-Höhe)
		spacing = overrides.spacing or 14, -- Abstand Icon ↔ Text
		margin = overrides.margin or { left = 16, right = 16, top = 4, bottom = 4 }, -- mehr Rand links

		-- Textstil (größer, fett + kursiv)
		font_size_scale = overrides.font_size_scale or 1.5,
		font_weight = overrides.font_weight or "bold",
		font_style = overrides.font_style or "italic",

		-- Farben
		bg = overrides.bg or "#27AE60",
		bg_hover = overrides.bg_hover or "#1F8F4A",
		fg = overrides.fg or "#FFFFFF",

		-- Form & Breite
		shape = overrides.shape or right_rounded,
		width_factor = overrides.width_factor or 4, -- mehr Breite (5× Wibar-Höhe)
		fixed_height = overrides.fixed_height ~= false,
	}
end

return S
