-- features/shell/menu/search/init.lua
-- Vorübergehender Ort für die komplette Searchbar-Logik.
-- Erwartet die vom Host (Footer) gebauten Widgets & Optionen und übernimmt
-- Fokus, Collapse/Expand, Prompt-Run und Styling (wie im alten Footer).

local awful = require("awful")
local gears = require("gears")

local M = {}

-- build({
--   prompt_widget, bg_box, width_ctl, stack,
--   hit_w, expand_w,
--   search_bg, search_fg, cursor,
--   history_path, on_search
-- })
function M.build(opts)
	assert(opts and opts.prompt_widget, "search.init: opts.prompt_widget required")
	assert(opts.bg_box and opts.width_ctl and opts.stack, "search.init: bg_box, width_ctl, stack required")

	local prompt = opts.prompt_widget
	local bg_box = opts.bg_box
	local width_ctl = opts.width_ctl
	local stack = opts.stack

	local HIT_W = opts.hit_w or 180
	local EXPAND_W = opts.expand_w or 180
	local SEARCH_BG = opts.search_bg or "#FFFFFF"
	local SEARCH_FG = opts.search_fg or "#000000"
	local CURSOR = opts.cursor or "#000000"

	local history_path = opts.history_path or (gears.filesystem.get_cache_dir() .. "/menu_search_history")
	local on_search = opts.on_search

	-- Cursorfarbe + Starttext
	if prompt.set_cursor_color then
		prompt:set_cursor_color(CURSOR)
	end
	if prompt.set_text then
		prompt:set_text("")
	end

	local collapsed = true
	local search_active = false

	local function prompt_node()
		return stack:get_children_by_id("prompt")[1]
	end

	local function apply_collapsed_style()
		collapsed = true
		bg_box.bg = "#00000000"
		bg_box.fg = SEARCH_FG
		width_ctl.width = HIT_W
		bg_box.forced_width = HIT_W
	end

	local function apply_expanded_style()
		collapsed = false
		bg_box.bg = SEARCH_BG
		bg_box.fg = SEARCH_FG
		width_ctl.width = EXPAND_W
		bg_box.forced_width = EXPAND_W
	end

	local function cancel_search()
		local pn = prompt_node()
		if pn then
			pn.visible = false
		end
		if prompt.set_text then
			prompt:set_text("")
		end
		search_active = false
		pcall(awful.keygrabber.stop)
		apply_collapsed_style()
	end

	local function focus_search()
		local pn = prompt_node()
		apply_expanded_style()
		if pn then
			pn.visible = true
		end
		if prompt.set_text then
			prompt:set_text("")
		end
		search_active = true

		awful.prompt.run({
			prompt = "",
			textbox = prompt,
			history_path = history_path,
			completion_callback = awful.completion.shell,
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

	-- Initial collapsed (sichtbare „Pille“ mit HIT_W)
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
