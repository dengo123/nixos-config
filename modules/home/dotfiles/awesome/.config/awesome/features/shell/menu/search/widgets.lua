-- features/shell/menu/search/widget.lua
-- Reines UI: Prefix-Label, Expand/Collapse, Prompt-Lifecycle.
-- Export:
--   local W = build(opts) -> {
--     root, refs={textbox,prefix_lbl,prompt},
--     set_mode(mode), begin_prompt(start_fn),
--     collapse(), is_prompt_running(), is_collapsed(),
--     on_left_click(fn), on_right_click(fn)
--   }

local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")

local M = {}

function M.build(opts)
	opts = opts or {}
	local H = opts.height_px or 16
	local WIDTH_EXP = opts.width_expanded or 220
	local WIDTH_COL = opts.width_collapsed or WIDTH_EXP

	local colors = opts.colors or {}
	local BG_ACTIVE = colors.bg or "#FFFFFF"
	local FG_ACTIVE = colors.fg or "#000000"
	local BG_COLLAPSED = colors.bg_collapsed or "#00000000"
	local CURSOR_BG = colors.cursor_bg or "#00000000"
	local CURSOR_FG = colors.cursor_fg or FG_ACTIVE

	local margins = opts.margins or {}
	local ML = margins.left or 10
	local MR = margins.right or 10
	local MT = margins.top or 4
	local MB = margins.bottom or 4
	local PREFIX_SP = opts.prefix_spacing or 8

	-- Prompt + Textbox
	local prompt = awful.widget.prompt({})
	local textbox = prompt.widget
	if textbox.set_align then
		textbox:set_align("left")
	end
	if textbox.set_valign then
		textbox:set_valign("center")
	end
	if textbox.set_text then
		textbox:set_text("")
	end
	textbox.bg = BG_ACTIVE
	textbox.fg = FG_ACTIVE
	textbox.bg_cursor = CURSOR_BG
	textbox.fg_cursor = CURSOR_FG

	-- fixes Präfix-Label (nicht editierbar)
	local prefix_lbl = wibox.widget({
		text = "",
		font = textbox.font or nil,
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- Innenlayout
	local inner_margin = wibox.widget({
		{
			prefix_lbl,
			textbox,
			spacing = PREFIX_SP,
			layout = wibox.layout.fixed.horizontal,
		},
		left = ML,
		right = MR,
		top = MT,
		bottom = MB,
		widget = wibox.container.margin,
	})

	local bg_box = wibox.widget({
		inner_margin,
		bg = BG_ACTIVE,
		fg = FG_ACTIVE,
		widget = wibox.container.background,
	})

	local height_ctl = wibox.widget({
		bg_box,
		strategy = "exact",
		height = H,
		widget = wibox.container.constraint,
	})

	local vcenter = wibox.widget({
		height_ctl,
		valign = "center",
		widget = wibox.container.place,
	})

	local width_ctl = wibox.widget({
		vcenter,
		strategy = "exact",
		width = WIDTH_EXP,
		widget = wibox.container.constraint,
	})

	local hleft = wibox.widget({
		width_ctl,
		halign = "left",
		widget = wibox.container.place,
	})

	local root = wibox.widget({
		hleft,
		layout = wibox.layout.fixed.horizontal,
	})

	-- State
	local state = {
		prompt_running = false,
		collapsed = true,
		saved_root_buttons = nil,
		ignore_next_root_click = false,
		mode = "local",
	}

	-- Styles
	local function apply_collapsed_style()
		state.collapsed = true
		bg_box.bg = BG_COLLAPSED
		inner_margin.left, inner_margin.right, inner_margin.top, inner_margin.bottom = 0, 0, 0, 0
		bg_box.forced_width = WIDTH_COL
		width_ctl.width = WIDTH_COL
		width_ctl:emit_signal("widget::layout_changed")
	end

	local function apply_expanded_style()
		state.collapsed = false
		bg_box.bg = BG_ACTIVE
		inner_margin.left, inner_margin.right, inner_margin.top, inner_margin.bottom = ML, MR, MT, MB
		bg_box.forced_width = WIDTH_EXP
		width_ctl.width = WIDTH_EXP
		width_ctl:emit_signal("widget::layout_changed")
	end

	local function detach_root_click_watcher()
		if state.saved_root_buttons then
			root.buttons(state.saved_root_buttons)
			state.saved_root_buttons = nil
		end
	end

	local function attach_root_click_watcher()
		if not state.saved_root_buttons then
			state.saved_root_buttons = root.buttons()
		end
		root.buttons(gears.table.join(awful.button({}, 1, function()
			if state.ignore_next_root_click then
				return
			end
			if state.prompt_running then
				pcall(function()
					awful.keygrabber.stop()
				end)
			end
			state.prompt_running = false
			apply_collapsed_style()
			detach_root_click_watcher()
		end)))
	end

	local function stop_prompt()
		if state.prompt_running then
			state.prompt_running = false
		end
		pcall(function()
			awful.keygrabber.stop()
		end)
		gears.timer.delayed_call(function()
			pcall(function()
				awful.keygrabber.stop()
			end)
		end)
		pcall(function()
			if textbox.set_text then
				textbox:set_text("")
			end
		end)
		prefix_lbl.text = ""
		detach_root_click_watcher()
	end

	local function collapse()
		stop_prompt()
		apply_collapsed_style()
	end

	-- Public controls
	local function set_mode(mode)
		state.mode = (mode == "web") and "web" or "local"
		prefix_lbl.text = (state.mode == "local") and "/" or "?"
	end

	local function begin_prompt(start_fn)
		apply_expanded_style()
		attach_root_click_watcher()
		-- verzögert starten (vermeidet Grabber-Kollisionen)
		gears.timer.delayed_call(function()
			state.prompt_running = true
			pcall(function()
				prompt.bg = BG_ACTIVE
			end)
			pcall(function()
				prompt.fg = FG_ACTIVE
			end)
			pcall(function()
				prompt.bg_cursor = CURSOR_BG
			end)
			pcall(function()
				prompt.fg_cursor = CURSOR_FG
			end)
			if start_fn then
				start_fn()
			end
		end)
	end

	-- Click hooks (werden in init.lua gesetzt)
	local left_click_cb, right_click_cb = nil, nil
	root:buttons(gears.table.join(
		awful.button({}, 1, function()
			state.ignore_next_root_click = true
			gears.timer.delayed_call(function()
				state.ignore_next_root_click = false
			end)
			if left_click_cb then
				left_click_cb()
			end
		end),
		awful.button({}, 3, function()
			state.ignore_next_root_click = true
			gears.timer.delayed_call(function()
				state.ignore_next_root_click = false
			end)
			if right_click_cb then
				right_click_cb()
			end
		end)
	))

	-- init: collapsed
	apply_collapsed_style()

	return {
		root = root,
		refs = { textbox = textbox, prefix_lbl = prefix_lbl, prompt = prompt },
		set_mode = set_mode,
		begin_prompt = begin_prompt,
		collapse = collapse,
		is_prompt_running = function()
			return state.prompt_running
		end,
		is_collapsed = function()
			return state.collapsed
		end,
		on_left_click = function(fn)
			left_click_cb = fn
		end,
		on_right_click = function(fn)
			right_click_cb = fn
		end,
	}
end

return M
