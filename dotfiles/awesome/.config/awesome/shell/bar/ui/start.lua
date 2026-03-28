-- ~/.config/awesome/shell/bar/ui/start.lua
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local S = {}

local runtime = {
	ui = {},
}

local DEFAULT_SYSTEM_ICON = gfs.get_configuration_dir() .. "ui/assets/flake.png"

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme()
	local ui = runtime.ui or {}
	return ui.theme or {}
end

local function with_size(font, size)
	font = type(font) == "string" and font ~= "" and font or "Sans"
	size = tonumber(size) or 10

	local base = font:gsub("%s+%d+%.?%d*$", "")
	return base .. " " .. tostring(size)
end

local function right_big_cap_factory(opts)
	local right_radius = opts.right_radius
	local left_radius = opts.left_radius or 0

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

function S.init(opts)
	opts = opts or {}
	runtime.ui = opts.ui or runtime.ui or {}

	local theme = resolved_theme()
	local colors = theme.colors or {}
	local fonts = theme.fonts or {}
	local icons = theme.icons or {}

	local system_icon = DEFAULT_SYSTEM_ICON
	if type(icons.system) == "string" and icons.system ~= "" then
		system_icon = icons.system
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

		font = with_size(fonts.ui_bold_italic, 15),

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
		bg = colors.start,
		bg_hover = colors.start_focus,
		fg = colors.text_invert,
	}

	return S
end

function S.get()
	local start_theme = beautiful.start or {}
	local colors = beautiful.start_colors or {}

	return {
		label = start_theme.label,

		icon = start_theme.system_icon,
		system_icon = start_theme.system_icon,
		icon_size = start_theme.icon_size,
		spacing = start_theme.spacing,
		margin = start_theme.margin,

		font = start_theme.font,

		width_factor = start_theme.width_factor,
		fixed_height = start_theme.fixed_height,

		right_radius = start_theme.right_radius,
		left_radius = start_theme.left_radius,
		shape = start_theme.shape,

		bg = colors.bg,
		bg_hover = colors.bg_hover,
		fg = colors.fg,
	}
end

return S
