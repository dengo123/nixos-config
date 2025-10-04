-- ~/.config/awesome/shell/launchers/run/view.lua
local wibox = require("wibox")
local gears = require("gears")

local V = {}

function V.build(ui, textbox)
	assert(textbox, "run.view: textbox required")
	assert(ui and ui.body_width, "run.view: ui.body_width required")
	assert(ui.height, "run.view: ui.height required")

	local pad_l = (ui.padding and ui.padding.left) or 12
	local pad_r = (ui.padding and ui.padding.right) or 12
	local pad_t = (ui.padding and ui.padding.top) or 6
	local pad_b = (ui.padding and ui.padding.bottom) or 6

	-- Hint (optional)
	local hint_row = nil
	if ui.hint and ui.hint.show ~= false and ui.hint.text and #ui.hint.text > 0 then
		local icon_w = nil
		if ui.hint.icon and #tostring(ui.hint.icon) > 0 then
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
			spacing = ui.hint.spacing or 6,
			layout = wibox.layout.fixed.horizontal,
		})
		hint_row = wibox.widget({
			{ inner, left = pad_l, right = pad_r, top = pad_t, bottom = 0, widget = wibox.container.margin },
			bg = ui.hint.bg or "#00000000",
			fg = ui.hint.fg or "#000000",
			widget = wibox.container.background,
		})
	end

	-- Linkes „Open:“ Label
	local open_lbl = wibox.widget({
		id = "open_lbl",
		markup = string.format("<span>%s</span>", ui.label_open_text or "Open:"),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})
	local label_w = tonumber(ui.label_open_width) or 64
	local open_lbl_w =
		wibox.widget({ open_lbl, strategy = "exact", width = label_w, widget = wibox.container.constraint })

	-- Prefix im Feld
	local prefix_lbl = wibox.widget({
		id = "prefix_lbl",
		text = "",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	-- Innenabstand in der weißen Box
	local row_in_field = wibox.widget({
		prefix_lbl,
		textbox,
		spacing = (ui.spacing or 8),
		layout = wibox.layout.fixed.horizontal,
	})

	local inner_margin = wibox.widget({
		row_in_field,
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

	-- Rahmen um die weiße Box (rechteckig, sauber clippen)
	local framed = wibox.widget({
		field_bg,
		shape = gears.shape.rectangle,
		shape_clip = true,
		shape_border_width = tonumber(ui.border_w) or 1,
		shape_border_color = ui.border_color or "#000000",
		bg = "#00000000",
		widget = wibox.container.background,
	})

	-- Höhe der Leiste fixieren
	local bar_h = wibox.widget({ framed, strategy = "exact", height = ui.height, widget = wibox.container.constraint })

	-- Breitensteuerung (vom Controller manipulierbar)
	local width_ctl = wibox.widget({
		bar_h,
		strategy = "exact",
		width = math.max(1, tonumber(ui.body_width) or 1),
		widget = wibox.container.constraint,
	})

	-- Fester Abstand zwischen Label und Feld
	local GAP = 10
	local spacer = wibox.widget({ strategy = "exact", width = GAP, widget = wibox.container.constraint })

	-- Hauptzeile exakt auf body_width, dann mittig
	local main_row = wibox.widget({
		open_lbl_w,
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
	local main_row_centered = wibox.widget({ main_row_fixed, halign = "center", widget = wibox.container.place })

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
			open_lbl = open_lbl,
			field_bg = field_bg, -- für bg/fg/cursor-Farben
			inner_margin = inner_margin, -- für Padding
			width_ctl = width_ctl, -- für expandierte Breite
		},
	}
end

return V
