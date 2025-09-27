-- features/shell/menu/dialogs/parts/layouts/rows.lua
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local U = require("features.shell.menu.dialogs.parts.layouts.util")
local W = require("features.shell.menu.dialogs.parts.widgets")

local R = {}

function R.make_icon_buttons(actions, th, size)
	local btns = {}
	for _, a in ipairs(actions or {}) do
		table.insert(
			btns,
			W.mk_icon_button({
				icon = a.icon,
				emoji = a.emoji,
				emoji_font = a.emoji_font,
				label = a.label,
				size = size or (U.compute_metrics(th).icon_size or 64),
				th = th,
				on_press = a.on_press,
			})
		)
	end
	return btns
end

local function section_header(label, th)
	return wibox.widget({
		{
			markup = string.format(
				"<span font='sans %d'><b>%s</b></span>",
				tonumber(th.section_title_size) or 18,
				label or ""
			),
			align = "left",
			valign = "center",
			widget = wibox.widget.textbox,
		},
		left = tonumber(th.section_pad_h) or 8,
		right = tonumber(th.section_pad_h) or 8,
		top = tonumber(th.section_pad_v) or 6,
		bottom = tonumber(th.section_pad_v) or 6,
		widget = wibox.container.margin,
	})
end

local function link_row(links, th)
	if not links or #links == 0 then
		return nil
	end
	local row = wibox.layout.fixed.horizontal()
	for i, lk in ipairs(links) do
		local btn = wibox.widget({
			{
				markup = string.format(
					"<span underline='single' font='sans %d'>%s</span>",
					tonumber(th.link_font_size) or 12,
					lk.label or ""
				),
				widget = wibox.widget.textbox,
			},
			widget = wibox.container.background,
		})
		btn:buttons(gears.table.join(awful.button({}, 1, function()
			if lk.on_press then
				lk.on_press()
			end
		end)))
		row:add(btn)
		if i < #links then
			row:add(wibox.widget({ markup = "  ·  ", widget = wibox.widget.textbox }))
		end
	end
	return wibox.widget({
		row,
		left = tonumber(th.section_pad_h) or 8,
		bottom = tonumber(th.section_pad_v) or 6,
		widget = wibox.container.margin,
	})
end

function R.rows_view(args, th, dims)
	args, th = args or {}, th or {}
	local cols = args.cols or 2
	local grid_sections = wibox.widget({
		homogeneous = false,
		expand = true,
		forced_num_cols = cols,
		spacing = tonumber(th.rows_section_spacing) or 14,
		layout = wibox.layout.grid,
	})

	local focusables = {}
	for _, sec in ipairs(args.sections or {}) do
		local icons_grid = wibox.widget({
			homogeneous = true,
			expand = true,
			forced_num_cols = tonumber(th.rows_icons_cols) or 2,
			spacing = tonumber(th.rows_icons_spacing) or 8,
			layout = wibox.layout.grid,
		})
		for _, b in ipairs(sec.items or {}) do
			icons_grid:add(b)
			table.insert(focusables, b)
		end
		local stack = wibox.widget({
			section_header(sec.title or "", th),
			icons_grid,
			link_row(sec.links, th),
			spacing = tonumber(th.rows_block_spacing) or 6,
			layout = wibox.layout.fixed.vertical,
		})
		local card = wibox.widget({
			{
				stack,
				left = tonumber(th.rows_card_pad_h) or 10,
				right = tonumber(th.rows_card_pad_h) or 10,
				top = tonumber(th.rows_card_pad_v) or 10,
				bottom = tonumber(th.rows_card_pad_v) or 10,
				widget = wibox.container.margin,
			},
			bg = th.rows_card_bg or "#00000015",
			shape = function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, tonumber(th.rows_card_radius) or 8)
			end,
			widget = wibox.container.background,
		})
		grid_sections:add(card)
	end

	local bg = th.rows_bg or th.panel_body_bg
	local root = wibox.widget({
		{
			grid_sections,
			left = tonumber(th.rows_pad_h or th.panel_pad_h) or 16,
			right = tonumber(th.rows_pad_h or th.panel_pad_h) or 16,
			top = tonumber(th.rows_pad_v or th.panel_pad_v) or 16,
			bottom = tonumber(th.rows_pad_v or th.panel_pad_v) or 16,
			widget = wibox.container.margin,
		},
		bg = bg,
		widget = wibox.container.background,
	})

	return root, focusables
end

return R
