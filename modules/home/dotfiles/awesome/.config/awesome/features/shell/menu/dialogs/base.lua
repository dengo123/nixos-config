-- ~/.config/awesome/features/shell/menu/dialogs/base.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

-- Zentrale Widgets + Theme
local Widgets = require("features.shell.menu.widgets")
local Theme = require("features.shell.menu.dialogs.theme")

-- Lib & Container
local Lib = require("features.shell.menu.lib")
local Containers = require("features.shell.menu.dialogs.containers")

-- Zwei Layouts: grid (Icons) & columns (Menüspalten)
local Layouts = {
	row = require("features.shell.menu.layouts.row"),
	columns = require("features.shell.menu.layouts.columns"),
}

local Base = {}
Base.layouts = Layouts

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

	-- ============================================================
	-- THEME-BRÜCKE (Dialog → Rows/Spalten)
	-- Verhindert, dass rows/columns auf Menü-Defaults zurückfallen.
	-- ============================================================
	-- Mappe body_* aus dem Dialog-Theme auf row_* (für rows.lua)
	if merged.body_bg and not merged.row_bg then
		merged.row_bg = merged.body_bg
	end
	if merged.body_fg and not merged.row_fg then
		merged.row_fg = merged.body_fg
	end

	-- Spaltenflächen (für columns.lua und seitenspezifische Rows)
	if not merged.left_bg then
		merged.left_bg = merged.row_bg
	end
	if not merged.left_fg then
		merged.left_fg = merged.row_fg
	end
	if not merged.right_bg then
		merged.right_bg = merged.row_bg
	end
	if not merged.right_fg then
		merged.right_fg = merged.row_fg
	end

	-- Hinweis: row_bg_hover wird in lib.theme aus row_bg abgeleitet,
	-- daher müssen wir es hier nicht setzen.

	return (Theme and Theme.get) and Theme.get(merged) or merged
end

-- ---------------------------------------------------------------------
-- Bequeme Wrapper für die beiden Layouts
-- ---------------------------------------------------------------------

-- ROW: Icon-Reihe/-Raster (liefert Widget + Fokusliste (linear))
function Base.layouts_row(actions, th, dims, get_close_ref)
	local geom = Layouts.row.compute_metrics(th, dims.w, dims.h)
	local row, items = Layouts.row.actions_row(actions or {}, th, geom, get_close_ref)
	return row, items
end

-- COLUMNS: n Spalten mit je eigener Palette/Rowhöhe
-- spec = { { key="left", width=..., palette={bg,fg,hover?}, row_h=?, items={...} }, ... }
function Base.layouts_columns(spec, th, dims)
	local api = Layouts.columns.build(spec or {}, th, { lib = Lib })
	local focus_lists = (api.get_focus_items and api:get_focus_items()) or {}
	return api.widget, focus_lists
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
--     mode = "grid" | "columns",
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
	local cancel_inner = Widgets.mk_cancel_button(pick(th.cancel_label, "Cancel"), nil, th)
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
	-- Fokussteuerung: nur "grid" und "columns"; sonst linearer Fallback
	----------------------------------------------------------------------
	local focus_cfg = opts.focus or {}
	local mode = focus_cfg.mode -- nil → Fallback linear

	if Lib and Lib.focus and focus_items then
		local stop_focus

		if mode == "row" then
			-- Erwartet lineare Liste von Buttons (mk_icon_button)
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
			-- Erwartet Liste-von-Listen: { {col1_row...}, {col2_row...}, ... }
			if type(Lib.focus.attach_columns_power_n) == "function" then
				-- Power-Liste leer (Dialog hat kein Footer-Power-Band)
				stop_focus = Lib.focus.attach_columns_power_n(focus_items, {}, th, {
					handle = handle,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
					start_col = focus_cfg.start_col or 1,
				})
			elseif type(Lib.focus.attach) == "function" then
				-- Fallback: flatten
				local linear = {}
				for _, col in ipairs(focus_items or {}) do
					for _, w in ipairs(col) do
						table.insert(linear, w)
					end
				end
				stop_focus = Lib.focus.attach(linear, th, {
					handle = handle,
					mouse_follow = (focus_cfg.mouse_follow ~= false),
				})
			end
		else
			-- simpler Fallback: linear (Body + Cancel ans Ende)
			local linear = {}
			if type(focus_items[1]) == "table" then
				-- könnte columns-shape sein → flatten
				for _, col in ipairs(focus_items) do
					for _, w in ipairs(col) do
						table.insert(linear, w)
					end
				end
			else
				for _, w in ipairs(focus_items) do
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
