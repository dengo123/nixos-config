-- ~/.config/awesome/shell/launchers/run/view.lua
local wibox = require("wibox")
local gears = require("gears")

local V = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function escape(text)
	return gears.string.xml_escape(tostring(text or ""))
end

local function build_hint_row(ui, pad_l, pad_r, pad_t)
	local hint = ui.hint
	if not (hint and hint.show ~= false and hint.text and #tostring(hint.text) > 0) then
		return nil
	end

	local gfs = require("gears.filesystem")
	local icon_w = nil
	local icon_size = assert(tonumber(hint.icon_size or hint.size), "run.view: ui.hint.icon_size/ui.hint.size required")

	if hint.icon_path and #tostring(hint.icon_path) > 0 then
		local path = tostring(hint.icon_path)
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

	if not icon_w and hint.icon and #tostring(hint.icon) > 0 then
		icon_w = wibox.widget({
			markup = string.format(
				"<span font='%s %d'>%s</span>",
				hint.font,
				assert(tonumber(hint.size), "run.view: ui.hint.size required"),
				escape(hint.icon)
			),
			align = "left",
			valign = "center",
			widget = wibox.widget.textbox,
		})
	end

	local txt_w = wibox.widget({
		markup = string.format(
			"<span font='%s %d'>%s</span>",
			hint.font,
			assert(tonumber(hint.size), "run.view: ui.hint.size required"),
			escape(hint.text)
		),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local inner = wibox.widget({
		icon_w or wibox.widget({}),
		txt_w,
		spacing = assert(tonumber(hint.icon_spacing), "run.view: ui.hint.icon_spacing required"),
		layout = wibox.layout.fixed.horizontal,
	})

	return wibox.widget({
		{
			inner,
			left = pad_l,
			right = pad_r,
			top = pad_t,
			bottom = 0,
			widget = wibox.container.margin,
		},
		bg = hint.bg,
		fg = hint.fg,
		widget = wibox.container.background,
	})
end

-- =========================================================================
-- Public API
-- =========================================================================

function V.build(ui, textbox)
	assert(textbox, "run.view: textbox required")
	assert(ui and tonumber(ui.body_width), "run.view: ui.body_width required")
	assert(tonumber(ui.height), "run.view: ui.height required")
	assert(ui.padding, "run.view: ui.padding required")

	-- ---------------------------------------------------------------------
	-- Config
	-- ---------------------------------------------------------------------

	local pad_l = assert(tonumber(ui.padding.left), "run.view: ui.padding.left required")
	local pad_r = assert(tonumber(ui.padding.right), "run.view: ui.padding.right required")
	local pad_t = assert(tonumber(ui.padding.top), "run.view: ui.padding.top required")
	local pad_b = assert(tonumber(ui.padding.bottom), "run.view: ui.padding.bottom required")

	local prefix_width = assert(tonumber(ui.prefix_width), "run.view: ui.prefix_width required")
	local prefix_font = assert(ui.prefix_font, "run.view: ui.prefix_font required")
	local prefix_size = assert(tonumber(ui.prefix_size), "run.view: ui.prefix_size required")

	local border_w = assert(tonumber(ui.border_w), "run.view: ui.border_w required")
	local border_color = assert(ui.border_color, "run.view: ui.border_color required")
	local bg_active = assert(ui.bg_active, "run.view: ui.bg_active required")
	local fg_active = assert(ui.fg_active, "run.view: ui.fg_active required")

	-- ---------------------------------------------------------------------
	-- Hint
	-- ---------------------------------------------------------------------

	local hint_row = build_hint_row(ui, pad_l, pad_r, pad_t)

	-- ---------------------------------------------------------------------
	-- Prefix
	-- ---------------------------------------------------------------------

	local prefix_lbl = wibox.widget({
		id = "prefix_lbl",
		font = string.format("%s %d", prefix_font, prefix_size),
		text = "",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local prefix_lbl_w = wibox.widget({
		prefix_lbl,
		strategy = "exact",
		width = prefix_width,
		widget = wibox.container.constraint,
	})

	-- ---------------------------------------------------------------------
	-- Field
	-- ---------------------------------------------------------------------

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
		bg = bg_active,
		fg = fg_active,
		widget = wibox.container.background,
	})

	local framed = wibox.widget({
		field_bg,
		shape = gears.shape.rectangle,
		shape_clip = true,
		shape_border_width = border_w,
		shape_border_color = border_color,
		bg = "#00000000",
		widget = wibox.container.background,
	})

	local bar_h = wibox.widget({
		framed,
		strategy = "exact",
		height = ui.height,
		widget = wibox.container.constraint,
	})

	local field_width = math.max(1, tonumber(ui.body_width) - prefix_width - 10)

	local width_ctl = wibox.widget({
		bar_h,
		strategy = "exact",
		width = field_width,
		widget = wibox.container.constraint,
	})

	-- ---------------------------------------------------------------------
	-- Main Row
	-- ---------------------------------------------------------------------

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
		width = math.max(1, tonumber(ui.body_width)),
		widget = wibox.container.constraint,
	})

	local main_row_centered = wibox.widget({
		main_row_fixed,
		halign = "center",
		widget = wibox.container.place,
	})

	-- ---------------------------------------------------------------------
	-- Layout
	-- ---------------------------------------------------------------------

	local vertical = wibox.widget({
		hint_row or nil,
		main_row_centered,
		spacing = (ui.hint and ui.hint.show ~= false and ui.hint.text and #ui.hint.text > 0)
				and assert(tonumber(ui.hint.spacing), "run.view: ui.hint.spacing required")
			or 0,
		layout = wibox.layout.fixed.vertical,
	})

	-- ---------------------------------------------------------------------
	-- View API
	-- ---------------------------------------------------------------------

	local api = {
		widget = vertical,
		parts = {
			textbox = textbox,
			prefix_lbl = prefix_lbl,
			field_bg = field_bg,
			inner_margin = inner_margin,
			width_ctl = width_ctl,
		},
	}

	function api.set_prefix(text)
		prefix_lbl.text = text or ""
	end

	function api.apply_active_style(style)
		style = style or {}

		field_bg.bg = style.bg_active or bg_active
		field_bg.fg = style.fg_active or fg_active

		inner_margin.left = style.left or pad_l
		inner_margin.right = style.right or pad_r
		inner_margin.top = style.top or pad_t
		inner_margin.bottom = style.bottom or pad_b

		width_ctl.width = style.width_expanded or width_ctl.width
		width_ctl:emit_signal("widget::layout_changed")

		if style.prefix_font or style.prefix_size then
			prefix_lbl.font =
				string.format("%s %d", style.prefix_font or prefix_font, tonumber(style.prefix_size) or prefix_size)
		end
	end

	return api
end

return V
