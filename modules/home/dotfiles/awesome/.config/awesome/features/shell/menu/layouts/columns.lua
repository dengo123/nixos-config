-- ~/.config/awesome/features/shell/menu/layouts/columns.lua
local wibox = require("wibox")
local P = require("features.shell.menu.widgets")

local Columns = {}

-- =========================
-- utils
-- =========================
local function shallow_copy(tbl)
	local out = {}
	for k, v in pairs(tbl or {}) do
		out[k] = v
	end
	return out
end

local function is_array(t)
	if type(t) ~= "table" then
		return false
	end
	local n = 0
	for k, _ in pairs(t) do
		if type(k) ~= "number" then
			return false
		end
		if k > n then
			n = k
		end
	end
	return n > 0
end

-- spec = { { items = {...}, ... }, { items = {...}, ... }, ... }
-- Kurzform erlaubt: spec = { {...}, {...} } mit jeder {...} selbst als Items-Liste
local function looks_like_spec_array(a)
	if not is_array(a) then
		return false
	end
	local first = a[1]
	if type(first) ~= "table" then
		return false
	end
	if type(first.items) == "table" and is_array(first.items) then
		return true
	end
	if is_array(first) then
		return true
	end
	return false
end

-- Robuste Moduserkennung zwischen:
--  - legacy2: build(left_items, right_items, theme, opts)
--  - spec:    build(spec_array, theme, opts)
local function detect_mode(a, b, c, d)
	-- Menü ruft: build(left_items, right_items, t, opts)
	if is_array(a) and is_array(b) then
		return { mode = "legacy2", left_items = a or {}, right_items = b or {}, t = c or {}, opts = d or {} }
	end
	-- Control Panel / generisch: build(spec_array, t, opts)
	if looks_like_spec_array(a) then
		return { mode = "spec", spec = a, t = b or {}, opts = c or {} }
	end
	-- Fallback: als 2-Spalten interpretieren
	return { mode = "legacy2", left_items = a or {}, right_items = b or {}, t = c or {}, opts = d or {} }
end

local function mk_column_widget(view, bg, forced_w)
	local inner = wibox.widget({
		{ view, margins = 0, widget = wibox.container.margin },
		bg = bg,
		widget = wibox.container.background,
	})
	if forced_w and tonumber(forced_w) then
		return wibox.widget({
			inner,
			forced_width = tonumber(forced_w),
			widget = wibox.container.constraint,
		})
	end
	return inner
end

-- =========================
-- Builder für N-Spalten-Descriptors
-- descs[i] = {
--   items = {...},
--   width = 0.33 (Ratio 0..1) | 240 (px) | nil,
--   row_h = number | nil,
--   palette = { bg=..., fg=..., hover=... } | nil,
--   opts = { ... } -- per-column opts für rows.lua
-- }
-- =========================
local function build_from_descriptors(descs, theme, top_opts)
	theme = theme or {}
	top_opts = top_opts or {}

	local col_widgets, col_focus = {}, {}
	local any_ratio = false
	local ratios = {}
	local spacing = tonumber(top_opts.spacing) or 0

	for i, d in ipairs(descs) do
		local items = d.items or {}
		local row_opts = shallow_copy(d.opts or {})
		row_opts.colors = row_opts.colors or d.palette
		row_opts.row_h = row_opts.row_h or d.row_h or theme.row_h
		row_opts.lib = row_opts.lib or top_opts.lib

		local view, focus = P.list_widget(items, theme, row_opts)

		local bg = (d.palette and d.palette.bg) or nil
		local forced_w = (d.width and tonumber(d.width) and not (d.width > 0 and d.width < 1)) and d.width or nil
		local ratio = (type(d.width) == "number" and d.width > 0 and d.width < 1) and d.width or nil
		if ratio then
			any_ratio = true
			ratios[i] = ratio
		end

		local colw = mk_column_widget(view, bg, forced_w)

		-- Spacing pro Spalte via Margins (funktioniert mit ratio & flex)
		if spacing > 0 then
			local left_pad = (i > 1) and math.floor(spacing / 2) or 0
			local right_pad = (i < #descs) and (spacing - math.floor(spacing / 2)) or 0
			colw = wibox.widget({
				colw,
				left = left_pad,
				right = right_pad,
				widget = wibox.container.margin,
			})
		end

		col_widgets[i] = colw
		col_focus[i] = focus or {}
	end

	-- Layout wählen
	local cols_layout
	if any_ratio then
		local ratio_layout = wibox.layout.ratio.horizontal()
		for _, w in ipairs(col_widgets) do
			ratio_layout:add(w)
		end
		local sum = 0
		for _, r in pairs(ratios) do
			sum = sum + r
		end
		if sum <= 0 then
			sum = 1
		end
		for idx, r in pairs(ratios) do
			ratio_layout:set_ratio(idx, r / sum)
		end
		cols_layout = ratio_layout
	else
		local flex = wibox.layout.flex.horizontal()
		for _, w in ipairs(col_widgets) do
			flex:add(w)
		end
		cols_layout = flex
	end

	-- Außen-Padding
	local cols_inner = wibox.widget({
		cols_layout,
		left = tonumber(top_opts.pad_l) or 0,
		right = tonumber(top_opts.pad_r) or 0,
		top = tonumber(top_opts.pad_t) or 0,
		bottom = tonumber(top_opts.pad_b) or 0,
		widget = wibox.container.margin,
	})

	local root = wibox.widget({
		cols_inner,
		widget = wibox.container.background,
	})

	-- API
	local api = { widget = root }
	local saved = { descs = descs, theme = theme, top_opts = top_opts }

	function api:set_col(i, items, per_col_opts)
		if not saved.descs[i] then
			return
		end
		saved.descs[i].items = items or {}
		saved.descs[i].opts = shallow_copy(per_col_opts or saved.descs[i].opts or {})
		-- Einfachheit: komplett neu bauen (wibox erlaubt Hot-Swap)
		local rebuilt = build_from_descriptors(saved.descs, saved.theme, saved.top_opts)
		api.widget = rebuilt.widget
		api.get_focus_items = rebuilt.get_focus_items
		api.set_left = rebuilt.set_left
		api.set_right = rebuilt.set_right
		api.set_col = rebuilt.set_col
	end

	-- Backward-Compat Helfer bei exakt 2 Spalten:
	if #descs == 2 then
		function api:set_left(items, per_col_opts)
			api:set_col(1, items, per_col_opts)
		end

		function api:set_right(items, per_col_opts)
			api:set_col(2, items, per_col_opts)
		end
	end

	function api:get_focus_items()
		-- Für Menü-Kompatibilität mit left/right bei 2 Spalten:
		if #col_focus == 2 then
			return { left = col_focus[1] or {}, right = col_focus[2] or {} }
		end
		-- 3+ Spalten: Listenform
		return col_focus
	end

	return api
end

-- =========================
-- Öffentliche build-Funktion (beide Modi)
-- =========================
function Columns.build(a, b, c, d)
	local m = detect_mode(a, b, c, d)

	if m.mode == "spec" then
		-- Normalisieren: Kurzform { {...}, {...} } → { {items={...}}, {items={...}} }
		local descs = {}
		for i, s in ipairs(m.spec or {}) do
			if s.items or not is_array(s) then
				descs[i] = {
					items = s.items or {},
					width = s.width,
					row_h = s.row_h,
					palette = s.palette,
					opts = s.opts,
				}
			else
				-- Kurzform: s ist schon eine Items-Liste
				descs[i] = { items = s }
			end
		end
		return build_from_descriptors(descs, m.t, m.opts)
	end

	-- ===== legacy 2-Spalten Pfad =====
	local t, opts = m.t, m.opts
	local left_opts = shallow_copy(opts.left_opts or {})
	local right_opts = shallow_copy(opts.right_opts or {})

	local left_view, left_focus = P.list_widget(m.left_items or {}, t, left_opts)
	local right_view, right_focus = P.list_widget(m.right_items or {}, t, right_opts)

	local left_bg = opts.left_bg
	local right_bg = opts.right_bg

	local left_col = mk_column_widget(left_view, left_bg, opts.left_w)
	local right_col = mk_column_widget(right_view, right_bg, opts.right_w)

	-- Spaltenabstand über Margins (kompatibel mit flex)
	local spacing = tonumber(opts.spacing) or 0
	if spacing > 0 then
		left_col = wibox.widget({ left_col, right = math.floor(spacing / 2), widget = wibox.container.margin })
		right_col =
			wibox.widget({ right_col, left = spacing - math.floor(spacing / 2), widget = wibox.container.margin })
	end

	local flex = wibox.layout.flex.horizontal()
	flex:add(left_col)
	flex:add(right_col)

	local cols_inner = wibox.widget({
		flex,
		left = tonumber(opts.pad_l) or 0,
		right = tonumber(opts.pad_r) or 0,
		top = tonumber(opts.pad_t) or 0,
		bottom = tonumber(opts.pad_b) or 0,
		widget = wibox.container.margin,
	})

	local root = wibox.widget({
		cols_inner,
		widget = wibox.container.background,
	})

	local api = { widget = root }

	function api:set_left(items, per_col_opts)
		local new_opts = shallow_copy(opts)
		new_opts.left_opts = per_col_opts or left_opts
		local new = Columns.build(items or {}, m.right_items or {}, t, new_opts)
		api.widget = new.widget
		api.get_focus_items = new.get_focus_items
		api.set_left = new.set_left
		api.set_right = new.set_right
	end

	function api:set_right(items, per_col_opts)
		local new_opts = shallow_copy(opts)
		new_opts.right_opts = per_col_opts or right_opts
		local new = Columns.build(m.left_items or {}, items or {}, t, new_opts)
		api.widget = new.widget
		api.get_focus_items = new.get_focus_items
		api.set_left = new.set_left
		api.set_right = new.set_right
	end

	function api:get_focus_items()
		return { left = left_focus or {}, right = right_focus or {} }
	end

	return api
end

return Columns
