-- ~/.config/awesome/shell/menu/layouts/columns.lua
local wibox = require("wibox")
local P = require("shell.menu.widgets")

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

-- Robuste Moduserkennung (legacy2 vs spec)
local function detect_mode(a, b, c, d)
	if is_array(a) and is_array(b) then
		return { mode = "legacy2", left_items = a or {}, right_items = b or {}, t = c or {}, opts = d or {} }
	end
	if looks_like_spec_array(a) then
		return { mode = "spec", spec = a, t = b or {}, opts = c or {} }
	end
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

-- Sichert, dass eine Hover-Farbe vorhanden ist
local function ensure_colors_hover(opt, theme, styler)
	if not opt or not opt.colors then
		return
	end
	local bg0 = opt.colors.bg or (theme and theme.row_bg) or "#00000000"
	opt.colors.hover = opt.colors.hover
		or (theme and theme.row_bg_hover)
		or (styler and styler.adjust and styler.adjust(bg0, -8))
		or bg0
end

-- =========================
-- Builder für N-Spalten-Descriptors (spec)
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

	local deps = top_opts.deps
	local styler = deps and deps.styler

	local col_widgets, col_focus = {}, {}
	local any_ratio = false
	local ratios = {}
	local spacing = tonumber(top_opts.spacing) or 0

	for i, d in ipairs(descs) do
		local items = d.items or {}
		local row_opts = shallow_copy(d.opts or {})
		row_opts.colors = row_opts.colors or d.palette
		row_opts.row_h = row_opts.row_h or d.row_h or theme.row_h
		row_opts.deps = row_opts.deps or deps

		-- Hover-Farbe sicherstellen, falls Palette übergeben wurde
		ensure_colors_hover(row_opts, theme, styler)

		local view, focus = P.list_widget(items, theme, row_opts)

		local bg = (d.palette and d.palette.bg) or nil
		local forced_w = (d.width and tonumber(d.width) and not (d.width > 0 and d.width < 1)) and d.width or nil
		local ratio = (type(d.width) == "number" and d.width > 0 and d.width < 1) and d.width or nil
		if ratio then
			any_ratio = true
			ratios[i] = ratio
		end

		local colw = mk_column_widget(view, bg, forced_w)

		-- Spacing pro Spalte via Margins
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
		local rebuilt = build_from_descriptors(saved.descs, saved.theme, saved.top_opts)
		api.widget = rebuilt.widget
		api.get_focus_items = rebuilt.get_focus_items
		api.set_left = rebuilt.set_left
		api.set_right = rebuilt.set_right
		api.set_col = rebuilt.set_col
	end

	-- Backward-Compat bei 2 Spalten:
	if #descs == 2 then
		function api:set_left(items, per_col_opts)
			api:set_col(1, items, per_col_opts)
		end

		function api:set_right(items, per_col_opts)
			api:set_col(2, items, per_col_opts)
		end
	end

	function api:get_focus_items()
		if #col_focus == 2 then
			return { left = col_focus[1] or {}, right = col_focus[2] or {} }
		end
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
				descs[i] = { items = s }
			end
		end
		return build_from_descriptors(descs, m.t, m.opts)
	end

	-- ===== legacy 2-Spalten Pfad =====
	local t, opts = m.t, m.opts
	local deps = opts.deps
	local styler = deps and deps.styler

	local left_opts = shallow_copy(opts.left_opts or {})
	local right_opts = shallow_copy(opts.right_opts or {})

	-- deps in Row-Opts pushen
	left_opts.deps = left_opts.deps or deps
	right_opts.deps = right_opts.deps or deps

	-- Hover-Farbe sicherstellen, wenn Palette/Colors übergeben wurde
	ensure_colors_hover(left_opts, t, styler)
	ensure_colors_hover(right_opts, t, styler)

	local left_view, left_focus = P.list_widget(m.left_items or {}, t, left_opts)
	local right_view, right_focus = P.list_widget(m.right_items or {}, t, right_opts)

	local left_bg = opts.left_bg
	local right_bg = opts.right_bg

	local left_col = mk_column_widget(left_view, left_bg, opts.left_w)
	local right_col = mk_column_widget(right_view, right_bg, opts.right_w)

	-- Spaltenabstand
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
		new_opts.deps = new_opts.deps or opts.deps
		local new = Columns.build(items or {}, m.right_items or {}, t, new_opts)
		api.widget = new.widget
		api.get_focus_items = new.get_focus_items
		api.set_left = new.set_left
		api.set_right = new.set_right
	end

	function api:set_right(items, per_col_opts)
		local new_opts = shallow_copy(opts)
		new_opts.right_opts = per_col_opts or right_opts
		new_opts.deps = new_opts.deps or opts.deps
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
