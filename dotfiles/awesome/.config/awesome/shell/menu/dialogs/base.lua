-- ~/.config/awesome/shell/menu/dialogs/base.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

-- Theme + Container
local Theme = require("shell.menu.dialogs.theme")
local Containers = require("shell.menu.dialogs.containers")

-- Zentrale Lib-Aggregation (focus, layout, term, placement, actions, apps, ...)
local Lib = require("shell.menu.lib")

local Base = {}

-- ---------------------------------------------------------------------
-- utils
-- ---------------------------------------------------------------------
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

-- Sicheres require (nil bei Fehler)
local function require_opt(path)
	local ok, mod = pcall(require, path)
	return ok and mod or nil
end

-- Cancel-Button-Factory beschaffen (robust gegen unterschiedliche Exports)
local function get_cancel_factory()
	-- bevorzugt neues Modul unter dialogs/widgets
	local mod = require_opt("shell.menu.dialogs.widgets.cancel")
	if mod then
		-- mögliche Export-Varianten abprüfen
		return mod.mk_cancel_button or mod.make or mod.new or mod[1]
	end
	-- Fallback: einfache Text-Schaltfläche
	return function(label, _icon, th)
		local tb = wibox.widget({
			markup = string.format("<b>%s</b>", label or "Cancel"),
			widget = wibox.widget.textbox,
		})
		local bg = wibox.widget({
			tb,
			fg = (th and th.row_fg) or "#000000",
			bg = (th and th.row_bg) or "#FFFFFF",
			widget = wibox.container.background,
		})
		return bg
	end
end

-- Theme-Resolver + Bridge in Menü-Theme
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

	-- ===== THEME-BRÜCKE: Dialog-Keys → Widgets/Rows/Columns =====
	-- rows erwarten row_*; Dialog liefert body_*
	if merged.body_bg and not merged.row_bg then
		merged.row_bg = merged.body_bg
	end
	if merged.body_fg and not merged.row_fg then
		merged.row_fg = merged.body_fg
	end

	-- columns/linke/rechte Seite defaulten auf row-Styles
	merged.left_bg = merged.left_bg or merged.row_bg
	merged.left_fg = merged.left_fg or merged.row_fg
	merged.right_bg = merged.right_bg or merged.row_bg
	merged.right_fg = merged.right_fg or merged.row_fg

	return (Theme and Theme.get) and Theme.get(merged) or merged
end

-- ---------------------------------------------------------------------
-- Bequeme Wrapper für die beiden Layouts (über Lib.layout)
-- ---------------------------------------------------------------------
-- ROW: Icon-Reihe/-Raster (liefert Widget + Fokusliste (linear))
function Base.layouts_row(actions, th, dims, get_close_ref)
	local L = Lib and Lib.layout
	if L and L.row and type(L.row.compute_metrics) == "function" and type(L.row.actions_row) == "function" then
		local geom = L.row.compute_metrics(th, dims.w, dims.h)
		local row, items = L.row.actions_row(actions or {}, th, geom, get_close_ref)
		return row, items
	end
	-- Fallback: leere horizontale Box
	return wibox.widget({ layout = wibox.layout.fixed.horizontal }), {}
end

-- COLUMNS: n Spalten mit je eigener Palette/Rowhöhe
-- spec = { { key="left", width=..., palette={bg,fg,hover?}, row_h=?, items={...} }, ... }
function Base.layouts_columns(spec, th, dims)
	local L = Lib and Lib.layout
	if L and L.columns and type(L.columns.build) == "function" then
		-- deps durchreichen, damit Rows/Widgets Zugriff auf Lib haben
		local api = L.columns.build(spec or {}, th, { deps = { lib = Lib }, dims = dims })
		local focus_lists = (api.get_focus_items and api:get_focus_items()) or {}
		return api.widget, focus_lists
	end
	-- Fallback: einfache vertikale Box
	return wibox.widget({ layout = wibox.layout.fixed.vertical }), {}
end

-- ---------------------------------------------------------------------
-- public: Dialog-Erzeuger
-- ---------------------------------------------------------------------
-- opts:
--   container = "power" | "panel" | ...
--   title
--   body_builder(th, dims, get_close) -> widget, focus_items
--   body_widget = widget (Alternative zu builder)
--   theme = { ... }
--   size = { w=?, h=? }
--   header_h / footer_h
--   popup = { width, height, show_root, close_on_backdrop, close_on_escape, placement, group, use_backdrop }
--   focus = {
--     mode = "row" | "columns",
--     cols = 4, start_index = 1, mouse_follow = true, start_col = 1
--   }
function Base.dialog(opts)
	opts = opts or {}
	local th = resolve_theme(opts.theme)

	-- Zielgröße
	local Wd = num(pick(opts.size and opts.size.w, th.dialog_w, 560), 560)
	local Hd = num(pick(opts.size and opts.size.h, th.dialog_h, 360), 360)

	-- Segmente
	local HEADER_H = num(pick(opts.header_h, th.header_h, 80), 80)
	local FOOTER_H = num(pick(opts.footer_h, th.footer_h, 80), 80)
	local BODY_H = math.max(0, Hd - HEADER_H - FOOTER_H)

	-- Pads (nur für Builder hilfreich)
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

	-- Cancel-Button (fokusfähig)
	local mk_cancel = get_cancel_factory()
	local cancel_inner = mk_cancel(pick(th.cancel_label, "Cancel"), nil, th)
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

	-- Container wählen und zusammenbauen
	local container_kind = opts.container or "power"
	local stack = Containers.build(container_kind, th, dims, {
		title = opts.title,
		body = body_core,
		cancel_btn = cancel_btn,
	})

	-- Popup-Optionen
	local p = opts.popup or {}
	local popup_width = num(p.width or Wd, Wd)
	local popup_height = num(p.height or Hd, Hd)

	local handle = Containers.popup.show(stack, th, {
		width = popup_width,
		height = popup_height,
		close_on_escape = pick(p.close_on_escape, true),
		close_on_backdrop = pick(p.close_on_backdrop, false),
		show_root = p.show_root,
		placement = p.placement,
		group = pick(p.group, "dialogs"),
		use_backdrop = p.use_backdrop,
	})

	------------------------------------------------------------------
	-- BRIDGE: jedes Close informiert zuerst das Menü-Overlay
	------------------------------------------------------------------
	local menu_api = rawget(_G, "__menu_api")
	local raw_close = handle.close
	handle.close = function(...)
		if menu_api and menu_api.hide_dialog then
			pcall(menu_api.hide_dialog, menu_api)
		end
		return raw_close(...)
	end

	-- Cancel-Verhalten + Fokusindikator fallback
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

	----------------------------------------------------------------------
	-- Fokussteuerung via Lib.focus: "row" | "columns" | linearer Fallback
	----------------------------------------------------------------------
	local focus_cfg = opts.focus or {}
	local mode = focus_cfg.mode -- nil → Fallback linear

	if Lib and Lib.focus and focus_items then
		local stop_focus

		if mode == "row" then
			if type(Lib.focus.attach_grid) == "function" then
				stop_focus = Lib.focus.attach_grid(focus_items, th, {
					cols = focus_cfg.cols or 4,
					start_index = focus_cfg.start_index or 1,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
				})
			elseif type(Lib.focus.attach) == "function" then
				stop_focus = Lib.focus.attach(focus_items, th, {
					handle = handle,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
				})
			end
		elseif mode == "columns" then
			local left, right
			if focus_items.left or focus_items.right then
				left, right = focus_items.left or {}, focus_items.right or {}
			else
				left, right = focus_items[1] or {}, focus_items[2] or {}
			end

			if type(Lib.focus.attach_columns_power) == "function" then
				stop_focus = Lib.focus.attach_columns_power(left, right, {}, th, {
					handle = handle,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
					start_side = (focus_cfg.start_col == 2) and "right" or "left",
				})
			elseif type(Lib.focus.attach) == "function" then
				local linear = {}
				for _, w in ipairs(left) do
					table.insert(linear, w)
				end
				for _, w in ipairs(right) do
					table.insert(linear, w)
				end
				stop_focus = Lib.focus.attach(linear, th, {
					handle = handle,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
				})
			end
		else
			-- simpler linearer Fallback
			local linear = {}
			if type(focus_items[1]) == "table" then
				for _, col in ipairs(focus_items or {}) do
					for _, w in ipairs(col) do
						table.insert(linear, w)
					end
				end
			else
				for _, w in ipairs(focus_items or {}) do
					table.insert(linear, w)
				end
			end
			table.insert(linear, cancel_btn)
			if type(Lib.focus.attach) == "function" then
				stop_focus = Lib.focus.attach(linear, th, {
					handle = handle,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
				})
			end
		end

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
