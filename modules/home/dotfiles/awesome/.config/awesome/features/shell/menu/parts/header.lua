-- ~/.config/awesome/features/shell/menu/components/header.lua
local wibox = require("wibox")
local P = require("features.shell.menu.widgets")

local Header = {}

-- user: { name="...", avatar="...", subtitle="..." }
-- t: { header_h, header_bg/fg, header_pad_*, header_spacing, avatar_size, avatar_radius, ... }
function Header.build(user, t)
	t = t or {}
	user = user or {}

	local H = t.header_h or 64
	local PAD_L = t.header_pad_l or 10
	local PAD_R = t.header_pad_r or 10
	local PAD_T = t.header_pad_t or 8
	local PAD_B = t.header_pad_b or 8
	local BG = t.header_bg or t.bg or "#235CDB"
	local FG = t.header_fg or t.fg or "#FFFFFF"

	-- verfügbare Innenhöhe (für Ratio-Berechnung der Inhalte)
	local avail_h = math.max(H - PAD_T - PAD_B, 1)

	-- Inhalt (Avatar + Text) aus widgets.lua
	local content = P.build_header_content(user, t, avail_h)

	local container = wibox.widget({
		{
			content.widget,
			left = PAD_L,
			right = PAD_R,
			top = PAD_T,
			bottom = PAD_B,
			widget = wibox.container.margin,
		},
		forced_height = H,
		bg = BG,
		fg = FG,
		widget = wibox.container.background,
	})

	local api = { widget = container }
	function api:set_user(name, avatar_path, subtitle)
		if content.set_user then
			content.set_user(name, avatar_path, subtitle)
		end
	end

	return api
end

return Header
