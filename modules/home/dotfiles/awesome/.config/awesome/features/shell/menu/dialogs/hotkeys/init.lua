-- ~/.config/awesome/features/shell/menu/dialogs/hotkeys/init.lua
local wibox = require("wibox")
local Base = require("features.shell.menu.dialogs.parts")

local M = {}

local HOTKEYS_SIZE = { w = 680, h = 460 } -- größer als power
local HEADER_H = 80
local FOOTER_H = 80

local function mk_text_body(th, _dims, _get_close, text)
	local tb = wibox.widget({
		markup = text or "No hotkeys defined.",
		align = "left",
		valign = "top",
		wrap = "word_char",
		widget = wibox.widget.textbox,
	})
	tb.font = th.mono_font or th.font or "monospace 10"
	return tb
end

function M.show_text(overrides, text)
	return Base.dialog({
		title = "Hotkeys",
		theme = overrides,
		size = HOTKEYS_SIZE,
		header_h = HEADER_H,
		footer_h = FOOTER_H,
		body_builder = function(th, dims, get_close)
			th.header_h, th.footer_h = dims.header_h, dims.footer_h
			return mk_text_body(th, dims, get_close, text)
		end,
	})
end

return M
