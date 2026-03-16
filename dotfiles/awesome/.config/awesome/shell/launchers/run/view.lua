-- ~/.config/awesome/shell/launchers/run/view.lua
local wibox = require("wibox")
local gears = require("gears")

local V = {}

function V.build(ui, textbox)
	assert(textbox, "run.view: textbox required")
	assert(ui and ui.body_width, "run.view: ui.body_width required")
	assert(ui.height, "run.view: ui.height required")

	-- =========================================================================
	-- Geometry
	-- =========================================================================

	local pad_l = (ui.padding and ui.padding.left) or 12
	local pad_r = (ui.padding and ui.padding.right) or 12
	local pad_t = (ui.padding and ui.padding.top) or 6
	local pad_b = (ui.padding and ui.padding.bottom) or 6

	-- =========================================================================
	-- Hint
	-- =========================================================================

	local hint_row = nil

	if ui.hint and ui.hint.show ~= false and ui.hint.text and #ui.hint.text > 0 then
		local gfs = require("gears.filesystem")
		local icon_w = nil
		local icon_size = tonumber(ui.hint.icon_size) or tonumber(ui.hint.size) or 20

		if ui.hint.icon_path and #tostring(ui.hint.icon_path) > 0 then
			local path = tostring(ui.hint.icon_path)
			local readable = gfs.file_readable(path)

			if readable then
				icon_w = wibox.widget({
					image = path,
					resize = true,
					forced_height = icon_size,
					forced_width = icon_size,
					valign = "center",
					halign = "center",
					widget = wibox.widget.imagebox,
				})
			end

			if not icon_w and readable then
				local surf
				if gears.surface and gears.surface.load_uncached then
					surf = gears.surface.load_uncached(path)
				elseif gears.surface and gears.surface.load then
					surf = gears.surface.load(path)
				end

				if surf then
					icon_w = wibox.widget({
						image = surf,
						resize = true,
						forced_height = icon_size,
						forced_width = icon_size,
						valign = "center",
						halign = "center",
						widget = wibox.widget.imagebox,
					})
				end
			end
		end

		if not icon_w and ui.hint.icon and #tostring(ui.hint.icon) > 0 then
			icon_w = wibox.widget({
				markup = string.format(
					"<span font='%s %d'>%s</span>",
					ui.hint.font or "Sans",
					tonumber(ui.hint.size) or 12,
					tostring(ui.hint.icon)
				),
				align = "left",
				valign = "center",
				widget = wibox.widget.textbox,
			})
		end

		local txt_w = wibox.widget({
			markup = string.format(
				"<span font='%s %d'>%s</span>",
				ui.hint.font or "Sans",
				tonumber(ui.hint.size) or 12,
				tostring(ui.hint.text)
			),
			align = "left",
			valign = "center",
			widget = wibox.widget.textbox,
		})

		local inner = wibox.widget({
			icon_w or wibox.widget({}),
			txt_w,
			spacing = ui.hint.icon_spacing or 6,
			layout = wibox.layout.fixed.horizontal,
		})

		hint_row = wibox.widget({
			{
				inner,
				left = pad_l,
				right = pad_r,
				top = pad_t,
				bottom = 0,
				widget = wibox.container.margin,
			},
			bg = ui.hint.bg or "#00000000",
			fg = ui.hint.fg or "#000000",
			widget = wibox.container.background,
		})
	end

	-- =========================================================================
	-- Prefix
	-- =========================================================================

	local prefix_lbl = wibox.widget({
		id = "prefix_lbl",
		text = "",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local prefix_width = tonumber(ui.prefix_width) or 64
	local prefix_lbl_w = wibox.widget({
		prefix_lbl,
		strategy = "exact",
		width = prefix_width,
		widget = wibox.container.constraint,
	})

	-- =========================================================================
	-- Input Field
	-- =========================================================================

	local field_row = wibox.widget({
		textbox,
		layout = wibox.layout.fixed.horizontal,
	})

	local inner_margin = wibox.widget({
		field_row,
		left = pad_l,
		right = pad_r,
		top = pad_t,
		bottom = pad_b,
		widget = wibox.container.margin,
	})

	local field_bg = wibox.widget({
		inner_margin,
		bg = ui.bg_active,
		fg = ui.fg_active,
		widget = wibox.container.background,
	})

	local framed = wibox.widget({
		field_bg,
		shape = gears.shape.rectangle,
		shape_clip = true,
		shape_border_width = tonumber(ui.border_w) or 1,
		shape_border_color = ui.border_color or "#000000",
		bg = "#00000000",
		widget = wibox.container.background,
	})

	local bar_h = wibox.widget({
		framed,
		strategy = "exact",
		height = ui.height,
		widget = wibox.container.constraint,
	})

	local field_width = math.max(1, (tonumber(ui.body_width) or 1) - prefix_width - 10)

	local width_ctl = wibox.widget({
		bar_h,
		strategy = "exact",
		width = field_width,
		widget = wibox.container.constraint,
	})

	-- =========================================================================
	-- Main Row
	-- =========================================================================

	local gap = 10
	local spacer = wibox.widget({
		widget = wibox.widget.separator,
		forced_width = gap,
		opacity = 0,
	})

	local main_row = wibox.widget({
		prefix_lbl_w,
		spacer,
		width_ctl,
		layout = wibox.layout.fixed.horizontal,
	})

	local main_row_fixed = wibox.widget({
		main_row,
		strategy = "exact",
		width = math.max(1, tonumber(ui.body_width) or 1),
		widget = wibox.container.constraint,
	})

	local main_row_centered = wibox.widget({
		main_row_fixed,
		halign = "center",
		widget = wibox.container.place,
	})

	-- =========================================================================
	-- Vertical Layout
	-- =========================================================================

	local vertical = wibox.widget({
		hint_row or nil,
		main_row_centered,
		spacing = (ui.hint and ui.hint.show ~= false and ui.hint.text and #ui.hint.text > 0) and (ui.hint.spacing or 6)
			or 0,
		layout = wibox.layout.fixed.vertical,
	})

	return {
		widget = vertical,
		parts = {
			textbox = textbox,
			prefix_lbl = prefix_lbl,
			field_bg = field_bg,
			inner_margin = inner_margin,
			width_ctl = width_ctl,
		},
	}
end

return V
