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

local function ui_api()
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
	assert(type(font) == "string" and font ~= "", "ui.theme.run: font fehlt")
	assert(tonumber(size) ~= nil, "ui.theme.run: size fehlt")

	local base = font:gsub("%s+%d+%.?%d*$", "")
	return base .. " " .. tostring(size)
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}
	runtime.ui = args.ui or {}
	return M
end

function M.get(overrides)
	overrides = overrides or {}

	local ui = ui_api()
	local theme = ui.theme or {}

	local C = theme.colors or {}
	local F = theme.fonts or {}
	local run_icon = icons.run or DEFAULT_RUN_ICON

	assert(type(C.primary) == "string" and C.primary ~= "", "ui.theme.run: theme.colors.primary fehlt")
	assert(type(C.surface) == "string" and C.surface ~= "", "ui.theme.run: theme.colors.surface fehlt")
	assert(
		type(C.surface_focus) == "string" and C.surface_focus ~= "",
		"ui.theme.run: theme.colors.surface_focus fehlt"
	)
	assert(type(C.white) == "string" and C.white ~= "", "ui.theme.run: theme.colors.white fehlt")
	assert(type(C.black) == "string" and C.black ~= "", "ui.theme.run: theme.colors.black fehlt")

	assert(type(F.ui) == "string" and F.ui ~= "", "ui.theme.run: theme.fonts.ui fehlt")
	assert(type(F.ui_bold) == "string" and F.ui_bold ~= "", "ui.theme.run: theme.fonts.ui_bold fehlt")

	local panel_defaults = {
		title = "Open",

		header_h = dpi(36),
		header_bg = C.primary,
		header_fg = C.white,
		header_font = with_size(F.ui_bold, 14),
		header_pad_l = dpi(12),
		header_pad_r = dpi(12),
		header_pad_v = dpi(6),

		body_bg = C.surface,
		body_fg = C.black,

		footer_h = dpi(80),
		footer_bg = C.surface,
		footer_fg = C.black,
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
		panel_border = C.surface_focus,
		panel_border_width = dpi(2),
	}

	local search_defaults = {
		sizes = {
			height = dpi(64),
			width_expanded = dpi(320),
		},

		colors = {
			bg_active = C.white,
			fg_active = C.black,
			cursor_bg = C.black,
			cursor_fg = C.white,
		},

		layout = {
			left = dpi(6),
			right = dpi(6),
			top = dpi(0),
			bottom = dpi(0),
			spacing = dpi(0),
		},

		border_w = dpi(1),
		border_color = C.black,

		prefix_width = dpi(62),
		prefix_font = with_size(F.ui, 11),

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
			fg = C.black,
			bg = C.surface,
			font = with_size(F.ui, 10),
			spacing = dpi(24),
		},
	}

	local button_defaults = {
		mode = "Mode",
		ok = "OK",
		cancel = "Cancel",
	}

	local panel = merge(panel_defaults, overrides.panel or {})
	local search = merge(search_defaults, overrides.search or {})
	local buttons = merge(button_defaults, overrides.buttons or {})

	return {
		panel = panel,
		search = search,
		buttons = buttons,
	}
end

M.resolve = M.get

return M
