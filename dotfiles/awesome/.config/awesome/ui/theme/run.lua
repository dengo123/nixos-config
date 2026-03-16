-- ~/.config/awesome/ui/theme/run.lua
local Colors = require("ui.colors")
local Helpers = require("ui.helpers")
local gfs = require("gears.filesystem")

local ASSETS = gfs.get_configuration_dir() .. "ui/assets/"

local M = {}

function M.get(overrides)
	overrides = overrides or {}

	-- =========================================================================
	-- Context
	-- =========================================================================

	local C = Colors.get()
	local H = Helpers
	local d = Helpers.dpi

	-- =========================================================================
	-- Panel
	-- =========================================================================

	local panel_defaults = {
		title = "Run",

		header_h = d(36),
		header_bg = C.blue_luna,
		header_fg = C.white,
		header_font = "Sans",
		header_font_size = d(14),
		header_pad_l = d(12),
		header_pad_r = d(12),
		header_pad_v = d(6),

		body_bg = C.creme,
		body_fg = C.black,
		footer_h = d(80),
		footer_bg = C.creme,
		footer_fg = C.black,

		width = d(420),
		height = d(240),
		pad_h = d(14),
		pad_v = d(12),

		panel_radius = d(12),
		panel_bg = C.blue_luna,
		panel_border = C.blue_luna,
		panel_border_width = d(2),
	}

	-- =========================================================================
	-- Search
	-- =========================================================================

	local search_defaults = {
		sizes = {
			height = d(24),
		},

		colors = {
			bg_active = C.white,
			fg_active = C.black,
			cursor_bg = C.black,
			cursor_fg = C.white,
		},

		layout = {
			left = d(6),
			right = d(6),
			top = d(0),
			bottom = d(0),
			spacing = d(0),
		},

		border_w = d(1),
		border_color = C.black,

		prefix_width = d(52),

		hint = {
			show = true,
			icon_path = ASSETS .. "Run_2001.png",
			icon_size = d(36),
			icon_spacing = d(4),
			text = "Type the name of a program, folder, document or Internet resource, and NixOS will open it for you.",
			fg = C.black,
			bg = C.creme,
			font = "Sans",
			size = d(10),
			spacing = d(24),
		},
	}

	-- =========================================================================
	-- Merge
	-- =========================================================================

	local panel = H.merge(panel_defaults, overrides.panel or {})
	local search = H.merge(search_defaults, overrides.search or {})

	return {
		panel = panel,
		search = search,
	}
end

M.resolve = M.get

return M
