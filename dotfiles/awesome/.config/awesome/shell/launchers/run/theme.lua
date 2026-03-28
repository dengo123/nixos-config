-- ~/.config/awesome/shell/launchers/run/theme.lua
local gfs = require("gears.filesystem")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi
local DEFAULT_RUN_ICON = gfs.get_configuration_dir() .. "ui/assets/Run_2001.png"

local M = {}

local runtime = {
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ui_theme()
	return runtime.ui or {}
end

local function merge(a, b)
	local out = {}

	for k, v in pairs(a or {}) do
		out[k] = v
	end

	for k, v in pairs(b or {}) do
		out[k] = v
	end

	return out
end

local function with_size(font, size)
	font = type(font) == "string" and font ~= "" and font or "Sans"
	size = tonumber(size) or 10

	local base = font:gsub("%s+%d+%.?%d*$", "")
	return base .. " " .. tostring(size)
end

local function merge_nested(base, override, keys)
	local out = merge(base, override)

	for _, key in ipairs(keys or {}) do
		out[key] = merge(base and base[key] or {}, override and override[key] or {})
	end

	return out
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(opts)
	opts = opts or {}
	runtime.ui = opts.ui or runtime.ui or {}
	return M
end

function M.get(overrides)
	overrides = overrides or {}

	local ui = ui_theme()
	local theme = ui.theme or {}

	local C = theme.colors or {}
	local F = theme.fonts or {}
	local I = theme.icons or {}

	local run_icon = I.run or DEFAULT_RUN_ICON
	if type(run_icon) == "string" and run_icon ~= "" and not run_icon:match("^/") then
		run_icon = gfs.get_configuration_dir() .. run_icon
	end

	local panel_defaults = {
		title = "Open",

		header_h = dpi(36),
		header_bg = C.primary,
		header_fg = C.text_invert,
		header_font = with_size(F.ui_bold, 14),
		header_pad_l = dpi(12),
		header_pad_r = dpi(12),
		header_pad_v = dpi(6),

		body_bg = C.surface,
		body_fg = C.text,

		footer_h = dpi(80),
		footer_bg = C.surface,
		footer_fg = C.text,
		footer_spacing = dpi(8),
		footer_pad_h = dpi(14),
		footer_pad_v = dpi(12),

		width = dpi(420),
		height = dpi(240),

		pad_h = dpi(14),
		pad_v = dpi(12),
		panel_pad_h = dpi(14),
		panel_pad_v = dpi(12),

		panel_radius = dpi(12),
		panel_bg = C.surface,
		panel_border = C.primary,
		panel_border_width = dpi(2),
	}

	local search_defaults = {
		sizes = {
			height = dpi(64),
			width_expanded = dpi(320),
		},

		colors = {
			bg_active = C.text_invert or C.background,
			fg_active = C.text or C.foreground,
			cursor_bg = C.text or C.foreground,
			cursor_fg = C.text_invert or C.background,
		},

		layout = {
			left = dpi(6),
			right = dpi(6),
			top = dpi(0),
			bottom = dpi(0),
			spacing = dpi(0),
		},

		border_w = dpi(1),
		border_color = C.text or C.black,

		prefix_width = dpi(62),
		prefix_font = F.ui,
		prefix_size = 11,

		prefix = {
			run_mode = "Run:",
			local_mode = "Files:",
			web_mode = "Browse:",
		},

		hint = {
			show = true,
			icon_path = run_icon,
			icon_size = dpi(36),
			icon_spacing = dpi(4),
			text = "Type the name of a program, folder, document or Internet resource, and Awesome will open it for you.",
			fg = C.text,
			bg = C.surface,
			font = F.ui,
			size = 10,
			spacing = dpi(28),
		},
	}

	local button_defaults = {
		mode = "Mode",
		ok = "OK",
		cancel = "Cancel",
	}

	local panel = merge(panel_defaults, overrides.panel or {})
	local search = merge_nested(search_defaults, overrides.search or {}, {
		"sizes",
		"colors",
		"layout",
		"prefix",
		"hint",
	})
	local buttons = merge(button_defaults, overrides.buttons or {})

	return {
		panel = panel,
		search = search,
		buttons = buttons,
	}
end

M.resolve = M.get

return M
