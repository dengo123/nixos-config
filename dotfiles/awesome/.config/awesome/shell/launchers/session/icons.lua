-- ~/.config/awesome/shell/launchers/session/icons.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function escape(text)
	return gears.string.xml_escape(tostring(text or ""))
end

local function build_square_shape(th)
	if th.icon_shape == "rounded" then
		local rounding = assert(tonumber(th.icon_rounding), "power.icons: icon_rounding fehlt/ungültig")
		return function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, rounding)
		end
	end

	return gears.shape.rectangle
end

-- ============================================================================
-- Public API
-- ============================================================================

function M.mk_icon_button(args)
	args = args or {}

	-- ------------------------------------------------------------------------
	-- Config
	-- ------------------------------------------------------------------------

	local th = args.th or {}
	local size = assert(tonumber(args.size), "power.icons: size fehlt/ungültig")

	local pad_icon = assert(tonumber(th.icon_pad), "power.icons: icon_pad fehlt/ungültig")
	local pad_cell = assert(tonumber(th.icon_cell_pad), "power.icons: icon_cell_pad fehlt/ungültig")
	local spacing = assert(tonumber(th.icon_spacing), "power.icons: icon_spacing fehlt/ungültig")

	local box_side = size + pad_icon * 2
	local square_shape = build_square_shape(th)

	-- ------------------------------------------------------------------------
	-- Icon
	-- ------------------------------------------------------------------------

	local icon_inner

	if type(args.icon) == "string" and #args.icon > 0 then
		icon_inner = wibox.widget({
			image = args.icon,
			resize = true,
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.imagebox,
		})
	else
		local emoji_char = args.emoji or "…"
		local emoji_font = args.emoji_font or ("sans " .. math.floor(size * 0.66))

		icon_inner = wibox.widget({
			markup = string.format("<span font='%s'>%s</span>", emoji_font, escape(emoji_char)),
			align = "center",
			valign = "center",
			forced_width = size,
			forced_height = size,
			widget = wibox.widget.textbox,
		})
	end

	local hover_square = wibox.widget({
		{
			{
				icon_inner,
				halign = "center",
				valign = "center",
				widget = wibox.container.place,
			},
			margins = pad_icon,
			widget = wibox.container.margin,
		},
		forced_width = box_side,
		forced_height = box_side,
		shape = square_shape,
		shape_clip = true,
		bg = "#00000000",
		widget = wibox.container.background,
	})

	-- ------------------------------------------------------------------------
	-- Label
	-- ------------------------------------------------------------------------

	local label_block = nil
	local label_h = 0

	if type(args.label) == "string" and #args.label > 0 then
		local label_size = assert(tonumber(th.icon_label_size), "power.icons: icon_label_size fehlt/ungültig")
		local label_leading = assert(tonumber(th.icon_label_leading), "power.icons: icon_label_leading fehlt/ungültig")
		local extra_w = assert(tonumber(th.icon_cell_extra_w), "power.icons: icon_cell_extra_w fehlt/ungültig")
		local label_w = box_side + extra_w

		local label_pad_top = tonumber(th.icon_label_pad_top) or 0
		local label_pad_bottom = tonumber(th.icon_label_pad_bottom) or 0

		label_h = math.ceil(label_size * label_leading) + label_pad_top + label_pad_bottom

		local label_widget = wibox.widget({
			markup = string.format(
				"<span font='sans %d' color='%s'>%s</span>",
				label_size,
				th.icon_label_color,
				escape(args.label)
			),
			align = "center",
			valign = "center",
			wrap = "none",
			ellipsize = "none",
			widget = wibox.widget.textbox,
		})

		label_block = wibox.widget({
			{
				label_widget,
				top = label_pad_top,
				bottom = label_pad_bottom,
				widget = wibox.container.margin,
			},
			strategy = "exact",
			width = label_w,
			widget = wibox.container.constraint,
		})
	end

	-- ------------------------------------------------------------------------
	-- Layout
	-- ------------------------------------------------------------------------

	local content = wibox.widget({
		{
			hover_square,
			halign = "center",
			valign = "center",
			widget = wibox.container.place,
		},
		label_block,
		spacing = (label_block and spacing or 0),
		layout = wibox.layout.fixed.vertical,
	})

	local clickable = wibox.widget({
		content,
		left = pad_cell,
		right = pad_cell,
		top = pad_cell,
		bottom = pad_cell,
		widget = wibox.container.margin,
	})

	-- ------------------------------------------------------------------------
	-- Mouse
	-- ------------------------------------------------------------------------

	hover_square:connect_signal("mouse::enter", function()
		hover_square.bg = th.icon_hover_bg
		hover_square.shape_border_width =
			assert(tonumber(th.icon_hover_bw), "power.icons: icon_hover_bw fehlt/ungültig")
		hover_square.shape_border_color = th.icon_hover_border
	end)

	hover_square:connect_signal("mouse::leave", function()
		hover_square.bg = "#00000000"
		hover_square.shape_border_width = 0
		hover_square.shape_border_color = nil
	end)

	if args.on_press then
		clickable:buttons(gears.table.join(awful.button({}, 1, function()
			args.on_press()
		end)))
	end

	-- ------------------------------------------------------------------------
	-- Focus API
	-- ------------------------------------------------------------------------

	clickable._hover_square = hover_square
	clickable._th = th
	clickable._on_press = args.on_press

	function clickable:set_focus(on, th2)
		local t = th2 or self._th or {}
		local sq = self._hover_square

		if on then
			sq.bg = t.icon_focus_bg
			sq.shape_border_width = assert(tonumber(t.icon_focus_bw), "power.icons: icon_focus_bw fehlt/ungültig")
			sq.shape_border_color = t.icon_focus_border
		else
			sq.bg = "#00000000"
			sq.shape_border_width = 0
			sq.shape_border_color = nil
		end
	end

	function clickable:activate()
		if self._on_press then
			self._on_press()
		end
	end

	clickable.mouse_enter_target = hover_square

	return clickable
end

return M
