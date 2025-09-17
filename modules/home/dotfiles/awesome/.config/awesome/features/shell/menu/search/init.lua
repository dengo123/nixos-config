-- features/shell/menu/search/init.lua
local awful = require("awful")
local gears = require("gears")
local ThemeMod = require("features.shell.menu.search.theme")

local M = {}

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

local function resolve_theme(shared, mod_tbl, overrides)
	shared = shared or {}
	mod_tbl = type(mod_tbl) == "table" and mod_tbl or {}
	overrides = overrides or {}

	-- abgeleitete Höhe: 1/3 der Footer-Höhe (Default 48 -> 16)
	local footer_h = pick(shared.footer_h, 48)
	local derived_h = math.max(1, math.floor(footer_h / 3 + 0.5))

	local t = {
		-- Farben: spezifisch > modul > generisch > fallback
		bg = pick(shared.search_bg, mod_tbl.search_bg, mod_tbl.bg, shared.bg, "#FFFFFF"),
		fg = pick(shared.search_fg, mod_tbl.search_fg, mod_tbl.fg, shared.fg, "#000000"),
		cursor = pick(shared.search_cursor, mod_tbl.cursor, "#000000"),

		-- Breiten
		hit_w = pick(shared.search_hit_w, mod_tbl.hit_w, 180),
		expand_w = pick(shared.search_w, mod_tbl.expand_w, mod_tbl.hit_w, 180),

		-- Paddings
		pad_l = pick(shared.search_pad_l, mod_tbl.pad_l, 10),
		pad_r = pick(shared.search_pad_r, mod_tbl.pad_r, 10),
		pad_t = pick(shared.search_pad_t, mod_tbl.pad_t, 2),
		pad_b = pick(shared.search_pad_b, mod_tbl.pad_b, 2),

		-- Höhe (aus Footer abgeleitet, falls nicht überschrieben)
		height = pick(shared.search_height, mod_tbl.height, derived_h),

		-- Prompt
		prompt_local = pick(shared.search_prompt_local, mod_tbl.prompt_local, "/"),
		prompt_web = pick(shared.search_prompt_web, mod_tbl.prompt_web, "?"),
	}

	-- user overrides oben drauf
	for k, v in pairs(overrides) do
		t[k] = v
	end
	-- safety: expand mindestens hit
	t.expand_w = t.expand_w or t.hit_w
	return t
end

-- opts:
--   prompt_widget (REQUIRED), prompt_node (REQUIRED), inner_margin?, bg_box (REQUIRED),
--   width_ctl (REQUIRED), height_ctl?, shared_theme?, search_overrides?, history_path?, on_search?
function M.build(opts)
	assert(opts and opts.prompt_widget, "search.init: opts.prompt_widget required")
	assert(opts.prompt_node and opts.bg_box and opts.width_ctl, "search.init: prompt_node, bg_box, width_ctl required")

	local T = resolve_theme(opts.shared_theme, ThemeMod, opts.search_overrides)

	local textbox = opts.prompt_widget
	local prompt_node = opts.prompt_node
	if prompt_node.get_children_by_id then
		prompt_node = prompt_node:get_children_by_id("prompt")[1] or prompt_node
	end

	local inner_m = opts.inner_margin
	local bg_box = opts.bg_box
	local width_ctl = opts.width_ctl
	local height_ctl = opts.height_ctl

	-- Theme → Layout anwenden
	if inner_m then
		inner_m.left, inner_m.right = T.pad_l, T.pad_r
		inner_m.top, inner_m.bottom = T.pad_t, T.pad_b
	end
	if height_ctl then
		height_ctl.height = T.height
	end
	if textbox.set_cursor_color then
		textbox:set_cursor_color(T.cursor)
	end
	if textbox.set_text then
		textbox:set_text("")
	end

	local history_path = opts.history_path or (gears.filesystem.get_cache_dir() .. "/menu_search_history")
	local on_search = opts.on_search

	local collapsed, search_active = true, false

	local function apply_width(w)
		width_ctl.width = w
		bg_box.forced_width = w
		-- Layout sicher refreshen, damit nichts “auf Max” bleibt
		width_ctl:emit_signal("widget::layout_changed")
		bg_box:emit_signal("widget::layout_changed")
	end

	local function apply_collapsed_style()
		collapsed = true
		bg_box.bg = "#00000000"
		bg_box.fg = T.fg
		apply_width(T.hit_w)
	end

	local function apply_expanded_style()
		collapsed = false
		bg_box.bg = T.bg
		bg_box.fg = T.fg
		apply_width(T.expand_w)
	end

	local function cancel_search()
		if prompt_node then
			prompt_node.visible = false
		end
		if textbox.set_text then
			textbox:set_text("")
		end
		search_active = false
		pcall(awful.keygrabber.stop)
		apply_collapsed_style()
	end

	local function focus_search()
		apply_expanded_style()
		if prompt_node then
			prompt_node.visible = true
		end
		if textbox.set_text then
			textbox:set_text("")
		end
		search_active = true

		awful.prompt.run({
			prompt = T.prompt_local or "/",
			textbox = textbox,
			history_path = history_path,
			completion_callback = awful.completion.shell,
			fg_cursor = T.fg,
			bg_cursor = T.bg,
			exe_callback = function(q)
				if q and #q > 0 and on_search then
					on_search(q)
				end
			end,
			done_callback = function()
				cancel_search()
			end,
		})
	end

	apply_collapsed_style()

	return {
		focus = focus_search,
		cancel = cancel_search,
		is_active = function()
			return search_active
		end,
		is_collapsed = function()
			return collapsed
		end,
	}
end

return M
