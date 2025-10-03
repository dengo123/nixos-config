-- ~/.config/awesome/ui/theme/start.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

local S = {}

function S.init(_) end

function S.get(overrides)
	overrides = overrides or {}

	local H = tonumber(beautiful.wibar_height) or 28
	local DEFAULT_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"

	-- Eigene Shape mit großem rechtem Cap; Parameter über overrides steuerbar
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

	-- Vollständige, theme-seitige Defaults
	local t = {
		-- Inhalt
		label = ("start"):lower(),
		icon = DEFAULT_ICON,

		-- Größen & Abstände
		icon_size = math.floor(H * 0.9),
		spacing = 2,
		margin = { left = 12, right = 4, top = 4, bottom = 4 },

		-- Textstil
		font_size_scale = 1.75,
		font_weight = "bold",
		font_style = "italic",

		-- Farben
		bg = "#27AE60",
		bg_hover = "#1F8F4A",
		fg = "#FFFFFF",

		-- Form & Breite
		width_factor = 4,
		fixed_height = true,

		-- Shape (mit parametrisierbaren Radien)
		right_radius = overrides.right_radius, -- z.B. 48/64/80…
		left_radius = overrides.left_radius or 0, -- z.B. 8
	}

	t.shape = overrides.shape
		or right_big_cap_factory({
			right_radius = t.right_radius,
			left_radius = t.left_radius,
		})

	return t
end

return S
