-- ~/.config/awesome/features/shell/menu/dialogs/hotkeys/init.lua
local wibox = require("wibox")
local Base = require("features.shell.menu.dialogs.parts")
local Loader = require("features.shell.menu.dialogs.hotkeys.hotkey_loader")

local M = {}

-- Abmessungen (fügen sich in deinen Base.dialog ein)
local HOTKEYS_SIZE = { w = 900, h = 640 }

-- kleine Helper
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- Einzelzeile: [ COMBO ]  [ Beschreibung ]
local function mk_item_row(th, combo, desc)
	local row = wibox.widget({
		{
			{
				markup = combo or "",
				align = "left",
				valign = "center",
				font = pick(th.mono_font, th.font, "monospace 10"),
				widget = wibox.widget.textbox,
			},
			{
				markup = desc or "",
				align = "left",
				valign = "center",
				font = pick(th.desc_font, th.font, nil),
				widget = wibox.widget.textbox,
			},
			spacing = pick(th.row_spacing, 12),
			layout = wibox.layout.fixed.horizontal,
		},
		top = pick(th.row_pad_v, 3),
		bottom = pick(th.row_pad_v, 3),
		widget = wibox.container.margin,
	})
	return row
end

-- Gruppe mit Titel + Items
local function mk_group(th, title, items)
	local col = wibox.layout.fixed.vertical()

	-- Gruppenüberschrift
	col:add(wibox.widget({
		{
			markup = "<b>" .. (title or "") .. "</b>",
			align = "left",
			valign = "center",
			font = pick(th.group_font, th.font, nil),
			widget = wibox.widget.textbox,
		},
		top = pick(th.group_title_pad_top, 2),
		bottom = pick(th.group_title_pad_bottom, 4),
		widget = wibox.container.margin,
	}))

	-- Einträge
	for _, it in ipairs(items or {}) do
		col:add(mk_item_row(th, it.combo, it.desc))
	end

	-- Abstand zur nächsten Gruppe
	col:add(wibox.widget({
		forced_height = pick(th.group_spacing, 8),
		widget = wibox.container.constraint,
	}))

	return col
end

-- Fallback-Ansicht als Text (z. B. wenn keine Keys vorhanden)
local function mk_text_body(th, _dims, _get_close, text)
	local tb = wibox.widget({
		markup = text or "No hotkeys defined.",
		align = "left",
		valign = "top",
		wrap = "word_char",
		widget = wibox.widget.textbox,
	})
	tb.font = pick(th.mono_font, th.font, "monospace 10")
	return tb
end

-- Öffentliche API: konsistent mit deiner Dialog-Registry
function M.hotkeys(overrides)
	-- Versuche explizit deine globalkeys zu holen
	local key_sets = {}
	do
		local ok_inp, input = pcall(require, "input")
		if ok_inp and input and input.keys and input.keys.globalkeys then
			table.insert(key_sets, input.keys.globalkeys)
		end
		-- Fallback: zusätzlich root.keys(), wenn vorhanden
		local ok_rk, rk = pcall(function()
			return root.keys()
		end)
		if ok_rk and type(rk) == "table" then
			table.insert(key_sets, rk)
		end
	end

	local data = Loader.scan({
		key_sets = (#key_sets > 0) and key_sets or nil,
		include_undesc = true, -- auf true setzen, falls du erstmal ALLES sehen willst
		-- labels = { Mod4 = "Win", ... }, -- optional
	})

	if not data or #data == 0 then
		return Base.dialog({
			title = "Hotkeys",
			theme = overrides,
			size = HOTKEYS_SIZE,
			body_builder = function(th, dims, get_close)
				th.header_h, th.footer_h = dims.header_h, dims.footer_h
				return mk_text_body(th, dims, get_close, "No hotkeys detected (missing descriptions?).")
			end,
		})
	end

	return Base.dialog({
		title = "Hotkeys",
		theme = overrides,
		size = HOTKEYS_SIZE,
		body_builder = function(th, dims, _get_close)
			th.header_h, th.footer_h = dims.header_h, dims.footer_h
			local body = wibox.layout.fixed.vertical()
			for _, g in ipairs(data) do
				body:add(mk_group(th, g.group, g.items))
			end
			return body
		end,
	})
end

-- Der frühere Text-Dialog bleibt verfügbar
function M.show_text(overrides, text)
	return Base.dialog({
		title = "Hotkeys",
		theme = overrides,
		size = HOTKEYS_SIZE,
		body_builder = function(th, dims, get_close)
			th.header_h, th.footer_h = dims.header_h, dims.footer_h
			return mk_text_body(th, dims, get_close, text)
		end,
	})
end

return M
