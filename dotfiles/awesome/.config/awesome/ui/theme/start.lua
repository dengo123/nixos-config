-- ~/.config/awesome/ui/theme/start.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi
local gfs = require("gears.filesystem")

local S = {}

-- interne Shape: großer rechter Cap, optional kleiner linker
local function right_big_cap_factory(args)
	local right_radius = args.right_radius
	local left_radius = args.left_radius or 0
	return function(cr, w, h)
		local R = right_radius or math.max(h, (beautiful.border_radius or 16) * 2)
		local rL = left_radius
		local top, bottom = 0, h

		if rL > 0 then
			cr:move_to(0, rL)
			cr:arc(rL, rL, rL, math.pi, 1.5 * math.pi)
		else
			cr:move_to(0, 0)
		end

		cr:line_to(w - R, top)
		local cx, cy = w - R, h / 2
		cr:arc(cx, cy, R, -math.pi / 2, math.pi / 2)

		if rL > 0 then
			cr:line_to(rL, bottom)
			cr:arc(rL, bottom - rL, rL, math.pi / 2, math.pi)
		else
			cr:line_to(0, bottom)
		end
		cr:close_path()
	end
end

function S.init(cfg)
	cfg = cfg or {}
	local C = cfg.colors or {}

	local H = tonumber(beautiful.wibar_height) or dpi(28)
	local DEFAULT_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"

	-- Maße & Stil
	beautiful.start = {
		-- Inhalt
		label = ("start"):lower(),
		icon = DEFAULT_ICON,

		-- Größen & Abstände
		icon_size = math.floor(H * 0.9),
		spacing = dpi(4),
		margin = { left = dpi(12), right = dpi(12), top = dpi(1), bottom = dpi(1) },

		-- Text
		font_size_scale = 1.75,
		font_weight = "bold",
		font_style = "italic",

		-- Breite & Höhe
		width_factor = 4,
		fixed_height = true,

		-- Radien
		right_radius = math.max(H, (beautiful.border_radius or dpi(16)) * 2),
		left_radius = 0,
	}

	beautiful.start.shape = right_big_cap_factory({
		right_radius = beautiful.start.right_radius,
		left_radius = beautiful.start.left_radius,
	})

	-- Farben rein aus Palette
	beautiful.start_colors = {
		bg = C.green, -- Haupt-Grün
		bg_hover = C.green_dark, -- abgedunkeltes Grün (Hover)
		fg = C.white,
	}
end

-- Konsumierbares Theme-Objekt für widgets/start.lua
function S.get()
	local ST = beautiful.start or {}
	local C = beautiful.start_colors or {}
	return {
		label = ST.label,
		icon = ST.icon,

		icon_size = ST.icon_size,
		spacing = ST.spacing,
		margin = ST.margin,

		font_size_scale = ST.font_size_scale,
		font_weight = ST.font_weight,
		font_style = ST.font_style,

		width_factor = ST.width_factor,
		fixed_height = ST.fixed_height,

		right_radius = ST.right_radius,
		left_radius = ST.left_radius,
		shape = ST.shape,

		-- Farben
		bg = C.bg,
		bg_hover = C.bg_hover,
		fg = C.fg,
	}
end

return S
