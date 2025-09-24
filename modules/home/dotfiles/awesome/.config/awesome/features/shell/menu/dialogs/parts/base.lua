-- ~/.config/awesome/features/shell/menu/dialogs/parts/base.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local Popup = require("features.shell.menu.dialogs.parts.popup")
local Theme = require("features.shell.menu.dialogs.parts.theme")
local Focus = require("features.shell.menu.lib.focus") -- ← Key-Fokus

local Base = {}

-- small utils
local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end
local function num(x, fb)
	x = tonumber(x)
	return x ~= nil and x or fb
end

-- theme resolve (unchanged idea)
local function shallow_merge(a, b)
	local out = {}
	if type(a) == "table" then
		for k, v in pairs(a) do
			out[k] = v
		end
	end
	if type(b) == "table" then
		for k, v in pairs(b) do
			out[k] = v
		end
	end
	return out
end
local function resolve_theme(overrides)
	local ov = {}
	if type(overrides) == "function" then
		local ok, t = pcall(overrides)
		if ok and type(t) == "table" then
			ov = t
		end
	elseif type(overrides) == "table" then
		ov = overrides
	end
	local ui_base = {}
	local ui = rawget(_G, "ui")
	if ui and type(ui.theme) == "table" then
		ui_base = ui.theme
	end
	local merged = (Theme and Theme.merge) and Theme.merge(ui_base, ov) or shallow_merge(ui_base, ov)
	return (Theme and Theme.get) and Theme.get(merged) or merged
end

-- public
function Base.dialog(opts)
	opts = opts or {}
	local th = resolve_theme(opts.theme)

	-- Größe & Segmente kommen von außen (mit sinnvollen Defaults)
	local Wd = num(pick(opts.size and opts.size.w, th.dialog_w, 560), 560)
	local Hd = num(pick(opts.size and opts.size.h, th.dialog_h, 360), 360)

	local HEADER_H = num(pick(opts.header_h, th.header_h, 80), 80)
	local FOOTER_H = num(pick(opts.footer_h, th.footer_h, 80), 80)
	local BODY_H = math.max(0, Hd - HEADER_H - FOOTER_H)

	-- Pads (Body-Innenabstand)
	local PAD_H = num(pick(th.pad_h, 16), 16)
	local PAD_V = num(pick(th.pad_v, 14), 14)

	-- dims an Builder reichen (praktisch für Icons/Text)
	local dims = {
		w = Wd,
		h = Hd,
		header_h = HEADER_H,
		footer_h = FOOTER_H,
		body_h = BODY_H,
		pad_h = PAD_H,
		pad_v = PAD_V,
	}

	-- Header
	local header = wibox.widget({
		{
			W.mk_header_content(opts.title or "", th),
			left = num(pick(th.header_pad_h, th.pad_h, 12), 12),
			right = num(pick(th.header_pad_h, th.pad_h, 12), 12),
			widget = wibox.container.margin,
		},
		bg = pick(th.header_bg, "#235CDB"),
		fg = pick(th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Body (Widget ODER Builder)  — kann jetzt auch focus_items liefern
	local body_core, focus_items
	local close_ref = function() end
	if type(opts.body_builder) == "function" then
		body_core, focus_items = opts.body_builder(th, dims, function()
			return close_ref
		end)
	elseif opts.body_widget then
		body_core = opts.body_widget
	else
		body_core = wibox.widget({ layout = wibox.layout.fixed.horizontal })
	end

	local body = wibox.widget({
		{
			{ body_core, halign = "center", valign = "center", widget = wibox.container.place },
			left = PAD_H,
			right = PAD_H,
			top = PAD_V,
			bottom = PAD_V,
			widget = wibox.container.margin,
		},
		bg = pick(th.body_bg, "#00000000"),
		fg = pick(th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Footer
	local cancel_btn = W.mk_cancel_button(pick(th.cancel_label, "Cancel"), nil, th)
	local target_w = th.cancel_width or (Wd > 0 and math.floor(Wd / 7) or nil)
	if target_w then
		cancel_btn = wibox.widget({
			cancel_btn,
			strategy = "exact",
			width = target_w,
			widget = wibox.container.constraint,
		})
	end

	local footer_right = wibox.widget({
		{ cancel_btn, halign = "right", valign = "center", widget = wibox.container.place },
		right = num(pick(th.footer_pad_h, th.pad_h, 12), 12),
		widget = wibox.container.margin,
	})

	local footer = wibox.widget({
		{
			footer_right,
			top = num(pick(th.footer_pad_v, 8), 8),
			bottom = num(pick(th.footer_pad_v, 8), 8),
			widget = wibox.container.margin,
		},
		bg = pick(th.footer_bg, "#235CDB"),
		fg = pick(th.footer_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- Fixhöhen/Stack (Header/Body/Footer sind fix, Body = Hd - Header - Footer)
	local header_fixed = wibox.widget({
		header,
		strategy = "exact",
		height = HEADER_H,
		widget = wibox.container.constraint,
	})
	local body_fixed = wibox.widget({
		body,
		strategy = "exact",
		height = BODY_H,
		widget = wibox.container.constraint,
	})
	local footer_fixed = wibox.widget({
		footer,
		strategy = "exact",
		height = FOOTER_H,
		widget = wibox.container.constraint,
	})

	local stack = wibox.widget({
		header_fixed,
		body_fixed,
		footer_fixed,
		layout = wibox.layout.fixed.vertical,
	})

	-- Popup
	local handle = Popup.show(stack, th, {
		width = Wd,
		height = Hd,
		close_on_escape = false,
		close_on_backdrop = false,
		group = "dialogs",
	})

	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		handle.close()
	end)))

	-- close_ref jetzt auf echte Close-Funktion binden
	close_ref = handle.close

	-- Tastatur-Fokus aktivieren (←/→ + Enter), falls Items vorhanden
	if type(focus_items) == "table" and #focus_items > 0 then
		local stop_focus = Focus.attach(focus_items, th, { handle = handle })
		-- Cleanup: Keygrabber stoppen, Fokus-Optik resetten
		if type(stop_focus) == "function" then
			local old_close = handle.close
			handle.close = function(...)
				pcall(stop_focus)
				return old_close(...)
			end
		end
	end

	return handle
end

Base.choice = Base.dialog
return Base
