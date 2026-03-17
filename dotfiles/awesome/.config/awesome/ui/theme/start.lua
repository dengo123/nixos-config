-- ~/.config/awesome/ui/theme/start.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local S = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function right_big_cap_factory(args)
	local right_radius = args.right_radius
	local left_radius = args.left_radius or 0

	return function(cr, w, h)
		local right_r = right_radius or math.max(h, (beautiful.border_radius or 16) * 2)
		local left_r = left_radius
		local top = 0
		local bottom = h

		if left_r > 0 then
			cr:move_to(0, left_r)
			cr:arc(left_r, left_r, left_r, math.pi, 1.5 * math.pi)
		else
			cr:move_to(0, 0)
		end

		cr:line_to(w - right_r, top)

		local cx = w - right_r
		local cy = h / 2
		cr:arc(cx, cy, right_r, -math.pi / 2, math.pi / 2)

		if left_r > 0 then
			cr:line_to(left_r, bottom)
			cr:arc(left_r, bottom - left_r, left_r, math.pi / 2, math.pi)
		else
			cr:line_to(0, bottom)
		end

		cr:close_path()
	end
end

-- =========================================================================
-- Public API
-- =========================================================================

function S.init(cfg)
	cfg = cfg or {}

	-- ---------------------------------------------------------------------
	-- Colors
	-- ---------------------------------------------------------------------

	local C = cfg.colors or {}

	-- ---------------------------------------------------------------------
	-- Geometry
	-- ---------------------------------------------------------------------

	local wibar_height = tonumber(beautiful.wibar_height) or dpi(28)
	local default_icon = gfs.get_configuration_dir() .. "ui/assets/flake.png"

	-- ---------------------------------------------------------------------
	-- Start
	-- ---------------------------------------------------------------------

	beautiful.start = {
		label = "start",
		icon = default_icon,

		icon_size = math.floor(wibar_height * 1),
		spacing = dpi(4),

		margin = {
			left = dpi(12),
			right = dpi(12),
			top = dpi(0),
			bottom = dpi(0),
		},

		font_size_scale = 1.75,
		font_weight = "bold",
		font_style = "italic",

		width_factor = 4,
		fixed_height = true,

		right_radius = math.max(wibar_height, (beautiful.border_radius or dpi(16)) * 2),
		left_radius = 0,
	}

	beautiful.start.shape = right_big_cap_factory({
		right_radius = beautiful.start.right_radius,
		left_radius = beautiful.start.left_radius,
	})

	-- ---------------------------------------------------------------------
	-- Colors
	-- ---------------------------------------------------------------------

	beautiful.start_colors = {
		bg = C.green,
		bg_hover = C.green_dark,
		fg = C.white,
	}
end

function S.get()
	-- ---------------------------------------------------------------------
	-- Theme State
	-- ---------------------------------------------------------------------

	local ST = beautiful.start or {}
	local C = beautiful.start_colors or {}

	-- ---------------------------------------------------------------------
	-- Theme Object
	-- ---------------------------------------------------------------------

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

		bg = C.bg,
		bg_hover = C.bg_hover,
		fg = C.fg,
	}
end

return S
