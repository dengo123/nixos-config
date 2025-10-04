-- ~/.config/awesome/ui/theme/windows.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local M = {}

function M.init(cfg)
	local C = (cfg and cfg.colors) or require("ui.colors").get()

	-- Rahmen/Farben
	beautiful.border_width = 3
	beautiful.border_radius = 10
	beautiful.border_normal = C.blue_light
	beautiful.border_focus = C.blue_luna

	-- Titlebar-Basis
	beautiful.titlebar_position = "top"
	beautiful.titlebar_height = 28
	beautiful.titlebar_bg_normal = C.blue_light
	beautiful.titlebar_bg_focus = C.blue_luna
	beautiful.titlebar_fg_normal = C.gray
	beautiful.titlebar_fg_focus = C.white

	------------------------------------------------------------------
	-- Gaps/Padding Defaults (werden von windowing.policies.gaps gelesen)
	------------------------------------------------------------------
	-- Innenabstand zwischen Clients (fair etc.)
	beautiful.useless_gap = dpi(4) -- dein Standard-Gap
	beautiful.gap_single_client = true -- Gap auch bei 1 Client behalten?

	-- Padding im MAX-Layout (Rand zum Bildschirm)
	beautiful.max_pad_on = true -- Padding im max-Layout aktivieren?
	beautiful.max_pad_same_as_gap = true -- Padding = useless_gap?
	-- Optional: eigenes Padding statt same_as_gap
	-- beautiful.max_padding = {
	--   top = dpi(8), right = dpi(8), bottom = dpi(8), left = dpi(8)
	-- }
end

-- Nur Stil, keine Logik:
function M.button_style(cfg)
	local C = (cfg and cfg.colors) or require("ui.colors").get()
	return {
		size = beautiful.titlebar_height or 28,
		spacing = 4,
		fg = C.white,
		fg_hover = C.gray,
		close = C.red,
		close_hover = C.pink,
	}
end

return M
