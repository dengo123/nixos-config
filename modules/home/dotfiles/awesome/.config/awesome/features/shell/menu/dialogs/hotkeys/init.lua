-- ~/.config/awesome/features/shell/menu/dialogs/hotkeys/init.lua
local wibox = require("wibox")
local awful = require("awful")

local Theme = require("features.shell.menu.dialogs.hotkeys.theme")
local Popup = require("features.shell.menu.dialogs.parts.popup")
local WParts = require("features.shell.menu.dialogs.parts.widgets")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

local function mk_header(title, th, on_close)
	local titlebox = wibox.widget({
		markup = string.format("<span font='sans %d'><b>%s</b></span>", pick(th.header_font_size, 12), title or ""),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local close_btn = WParts.mk_cancel_button("×", on_close, th)

	local bar = wibox.widget({
		{
			{ titlebox, left = pick(th.header_pad_h, th.pad_h, 12), widget = wibox.container.margin },
			{
				close_btn,
				halign = "right",
				valign = "center",
				widget = wibox.container.place,
			},
			layout = wibox.layout.align.horizontal,
		},
		bg = pick(th.header_bg, "#1D4ED8"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	return wibox.widget({
		bar,
		strategy = "exact",
		height = pick(th.header_h, 28),
		widget = wibox.container.constraint,
	})
end

local function mk_footer(th, on_close)
	local row = wibox.layout.fixed.horizontal()
	row.spacing = pick(th.footer_btn_spacing, 8)
	row:add(WParts.mk_cancel_button("Close", on_close, th))

	local placed = wibox.widget({
		row,
		halign = "right",
		valign = "center",
		widget = wibox.container.place,
	})

	return wibox.widget({
		{
			placed,
			left = pick(th.pad_h, 12),
			right = pick(th.pad_h, 12),
			top = pick(th.footer_pad_v, 8),
			bottom = pick(th.footer_pad_v, 8),
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#0B1020"),
		fg = pick(th.body_fg, "#E5EAF5"),
		widget = wibox.container.background,
	})
end

-- API: hotkeys({ title?, width?, height?, anchor? ... } + alle Theme-Overrides)
function M.hotkeys(overrides)
	-- 1) Hotkeys-Theme aus Defaults + externen Overrides
	--    (Overrides kommen z.B. aus Columns via dialog_overrides)
	local th = Theme.get(overrides)

	local W = pick(th.dialog_w, 900)
	local H = pick(th.dialog_h, 560)

	local handle
	local function close()
		if handle and handle.close then
			handle.close()
		end
	end

	-- Inhalt: Awesome Hotkeys-Widget
	local hk_widget = require("awful.hotkeys_popup.widget").new({
		width = W - 2 * pick(th.pad_h, 12), -- etwas Innenmargin einkalkulieren
	})

	local header = mk_header(pick(th.title, overrides and overrides.title, "Keyboard Shortcuts"), th, close)

	local body = wibox.widget({
		{
			hk_widget,
			left = pick(th.pad_h, 12),
			right = pick(th.pad_h, 12),
			top = pick(th.pad_v, 10),
			bottom = pick(th.pad_v, 10),
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#0B1020"),
		fg = pick(th.body_fg, "#E5EAF5"),
		widget = wibox.container.background,
	})

	local footer = mk_footer(th, close)

	local root = wibox.widget({
		header,
		body,
		footer,
		layout = wibox.layout.fixed.vertical,
	})

	-- 2) Rendern über deinen parts/popup
	handle = Popup.show(root, {
		dialog_radius = pick(th.dialog_radius, 8),
		dialog_border_width = pick(th.dialog_border_width, 2),
		dialog_border = pick(th.dialog_border, th.header_bg or "#1D4ED8"),
		dialog_bg = pick(th.dialog_bg, th.body_bg or "#0B1020"),
		dialog_backdrop = pick(th.dialog_backdrop, "#00000088"),
	}, {
		width = W,
		height = H,
		anchor = th.anchor or (overrides and overrides.anchor),
	})

	return handle
end

return M
