-- ~/.config/awesome/features/shell/menu/dialogs/parts/init.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local W = require("features.shell.menu.dialogs.parts.widgets")
local Popup = require("features.shell.menu.dialogs.parts.popup")
local Theme = require("features.shell.menu.dialogs.parts.theme")
local Lib = require("features.shell.menu.lib")
local Containers = require("features.shell.menu.dialogs.parts.containers")
local Icons = require("features.shell.menu.dialogs.parts.icons") -- <<< NEU

local Base = {}

-- re-export so callers can do: Base.icons.... (oder Helper unten)
Base.icons = Icons -- <<< NEU

-- optional: bequeme Wrapper (nutzen das re-exportierte Icons)
function Base.icons_row(actions, th, dims, get_close_ref) -- <<< NEU
	local geom = Icons.compute_metrics(th, dims.w, dims.h)
	return Icons.actions_row(actions, th, geom, get_close_ref)
end

function Base.icons_grid(actions, th, dims, opts, get_close_ref) -- <<< NEU
	opts = opts or {}
	opts.dialog_w = dims.w
	opts.dialog_h = dims.h
	return Icons.actions_grid(actions, th, opts, get_close_ref)
end

-- utils -----------------------------------------------------------------
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
-- utils -----------------------------------------------------------------

-- public ----------------------------------------------------------------
function Base.dialog(opts)
	opts = opts or {}
	local th = resolve_theme(opts.theme)

	-- Zielgröße
	local Wd = num(pick(opts.size and opts.size.w, th.dialog_w, 560), 560)
	local Hd = num(pick(opts.size and opts.size.h, th.dialog_h, 360), 360)

	-- Standard-Segmenthöhen (Container darf sie später anpassen)
	local HEADER_H = num(pick(opts.header_h, th.header_h, 80), 80)
	local FOOTER_H = num(pick(opts.footer_h, th.footer_h, 80), 80)
	local BODY_H = math.max(0, Hd - HEADER_H - FOOTER_H)

	-- Pads
	local PAD_H = num(pick(th.pad_h, 16), 16)
	local PAD_V = num(pick(th.pad_v, 14), 14)

	local dims = {
		w = Wd,
		h = Hd,
		header_h = HEADER_H,
		footer_h = FOOTER_H,
		body_h = BODY_H,
		pad_h = PAD_H,
		pad_v = PAD_V,
	}

	-- Body erstellen (Widget oder Builder)
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

	-- Cancel-Button (fokusfähig); Darstellung ist Container-abhängig
	local cancel_inner = W.mk_cancel_button(pick(th.cancel_label, "Cancel"), nil, th)
	local target_w = th.cancel_width or (Wd > 0 and math.floor(Wd / 7) or nil)
	if target_w then
		cancel_inner = wibox.widget({
			cancel_inner,
			strategy = "exact",
			width = target_w,
			widget = wibox.container.constraint,
		})
	end
	local cancel_btn = wibox.widget({ cancel_inner, bg = "#00000000", widget = wibox.container.background })

	-- Container wählen und zusammensetzen
	local container_kind = opts.container or "power" -- default = alter Power-Frame
	local stack = Containers.build(container_kind, th, dims, {
		title = opts.title,
		body = body_core,
		cancel_btn = cancel_btn,
	})

	-- Popup öffnen
	local handle = Popup.show(stack, th, {
		width = Wd,
		height = Hd,
		close_on_escape = pick(opts.close_on_escape, false),
		close_on_backdrop = pick(opts.close_on_backdrop, false),
		group = pick(opts.group, "dialogs"),
	})

	-- Cancel-Verhalten
	if not cancel_btn.set_focus then
		function cancel_btn:set_focus(on)
			local on_bg = pick(th.cancel_bg_hover, th.row_bg_hover, th.bg_focus, "#FFFFFF22")
			local off_bg = pick(th.cancel_bg, "#00000000")
			self.bg = on and on_bg or off_bg
		end
	end
	if not cancel_btn.activate then
		function cancel_btn:activate()
			handle.close()
		end
	end
	cancel_btn.mouse_enter_target = cancel_btn
	cancel_btn:buttons(gears.table.join(awful.button({}, 1, function()
		handle.close()
	end)))

	-- Buildern die echte Close-Funktion geben
	close_ref = handle.close

	-- Fokussteuerung (Body + Cancel)
	if
		type(focus_items) == "table"
		and #focus_items > 0
		and Lib
		and Lib.focus
		and type(Lib.focus.attach_dialog) == "function"
	then
		local stop_focus = Lib.focus.attach_dialog(focus_items, cancel_btn, th, {
			handle = handle,
			mouse_follow = true,
		})
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
