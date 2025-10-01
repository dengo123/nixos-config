local beautiful = require("beautiful")
local gears = require("gears")
local gfs = require("gears.filesystem")

local S = {}

function S.init(_) end

function S.get(overrides)
	overrides = overrides or {}

	local DEFAULT_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"
	local H = tonumber(beautiful.wibar_height) or 28

	-- Eigene Shape: links optional rund, rechts großer Kreisbogen (Radius > h/2 möglich)
	local function right_big_cap(cr, w, h)
		local R = overrides.right_radius or math.max(h, (beautiful.border_radius or 16) * 2) -- großer rechter Radius
		local rL = overrides.left_radius or 0 -- 0 = links gerade; z.B. 8 für leichte Rundung
		local top = 0
		local bottom = h

		-- Start links oben (mit kleiner/linker Rundung)
		if rL > 0 then
			cr:move_to(0, rL)
			cr:arc(rL, rL, rL, math.pi, 1.5 * math.pi) -- linke obere Ecke
		else
			cr:move_to(0, 0)
		end

		-- Oberkante bis vor den rechten Bogen
		cr:line_to(w - R, top)

		-- Rechter „Cap“: Kreisbogen (Zentrum außerhalb, Radius R)
		-- Mittelpunkt des Bogens:
		local cx, cy = w - R, h / 2
		cr:arc(cx, cy, R, -math.pi / 2, math.pi / 2)

		-- Unterkante zurück
		if rL > 0 then
			cr:line_to(rL, bottom)
			cr:arc(rL, bottom - rL, rL, math.pi / 2, math.pi) -- linke untere Ecke
		else
			cr:line_to(0, bottom)
		end

		cr:close_path()
	end

	return {
		-- Inhalt
		label = (overrides.label or "start"):lower(),
		icon = overrides.icon or DEFAULT_ICON,

		-- Größen & Abstände
		icon_size = overrides.icon_size or math.floor(H * 0.9),
		spacing = overrides.spacing or 12,
		margin = overrides.margin or { left = 16, right = 16, top = 4, bottom = 4 },

		-- Textstil
		font_size_scale = overrides.font_size_scale or 1.6,
		font_weight = overrides.font_weight or "bold",
		font_style = overrides.font_style or "italic",

		-- Farben
		bg = overrides.bg or "#27AE60",
		bg_hover = overrides.bg_hover or "#1F8F4A",
		fg = overrides.fg or "#FFFFFF",

		-- Form & Breite
		shape = overrides.shape or right_big_cap, -- << unsere Custom-Shape
		width_factor = overrides.width_factor or 4,
		fixed_height = overrides.fixed_height ~= false,

		-- Zusatz-Parameter für die Shape (kannst du in cfg.start überschreiben)
		right_radius = overrides.right_radius, -- z.B. 48, 64, 80 … (größer als h/2 ergibt „Kreisausschnitt“)
		left_radius = overrides.left_radius or 0, -- z.B. 8, wenn links weich
	}
end

return S
