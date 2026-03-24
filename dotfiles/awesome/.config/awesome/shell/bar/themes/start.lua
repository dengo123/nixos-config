-- ~/.config/awesome/shell/bar/themes/start.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local S = {}

local DEFAULT_SYSTEM_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme(args)
	local ui = (args and args.ui) or {}
	return ui.theme or {}
end

local function with_size(font, size)
	font = type(font) == "string" and font ~= "" and font or "Sans"
	size = tonumber(size) or 10

	local base = font:gsub("%s+%d+%.?%d*$", "")
	return base .. " " .. tostring(size)
end

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

function S.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}
	local F = theme.fonts or {}
	local I = theme.icons or {}

	local system_icon = DEFAULT_SYSTEM_ICON
	if type(I.system) == "string" and I.system ~= "" then
		system_icon = I.system
	end

	local wibar_height = tonumber(beautiful.wibar_height) or dpi(28)

	beautiful.start = {
		label = "Start",
		system_icon = system_icon,

		icon_size = math.floor(wibar_height * 1.2),
		spacing = dpi(4),

		margin = {
			left = dpi(12),
			right = dpi(12),
			top = dpi(0),
			bottom = dpi(0),
		},

		font = with_size(F.ui_bold_italic, 15),

		width_factor = 4,
		fixed_height = true,

		right_radius = math.max(wibar_height, (beautiful.border_radius or dpi(16)) * 2),
		left_radius = 0,
	}

	beautiful.start.shape = right_big_cap_factory({
		right_radius = beautiful.start.right_radius,
		left_radius = beautiful.start.left_radius,
	})

	beautiful.start_colors = {
		bg = C.start,
		bg_hover = C.start_focus,
		fg = C.text_invert,
	}
end

function S.get()
	local ST = beautiful.start or {}
	local C = beautiful.start_colors or {}

	return {
		label = ST.label,

		icon = ST.system_icon,
		system_icon = ST.system_icon,
		icon_size = ST.icon_size,
		spacing = ST.spacing,
		margin = ST.margin,

		font = ST.font,

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
