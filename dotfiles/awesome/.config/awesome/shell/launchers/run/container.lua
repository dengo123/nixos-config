-- ~/.config/awesome/shell/launchers/run/container.lua
local wibox = require("wibox")
local gears = require("gears")

local M = {}

-- ============================================================================
-- Helpers
-- ============================================================================

local function pick(...)
	for i = 1, select("#", ...) do
		local v = select(i, ...)
		if v ~= nil then
			return v
		end
	end
end

-- ============================================================================
-- Build
-- ============================================================================

-- build(th, dims, {
--   title,
--   body,
--   footer_buttons = { ... },
--   footer,
-- })
function M.build(th, dims, w)
	-- =========================================================================
	-- Geometry
	-- =========================================================================

	local radius = tonumber(pick(th.panel_radius, 12))
	local border_w = tonumber(pick(th.panel_border_width, 2))
	local border_color = pick(th.panel_border, th.header_bg)

	local header_h = tonumber(pick(th.header_h, 28))
	local footer_h = tonumber(pick(th.footer_h, dims.footer_h))

	-- =========================================================================
	-- Header
	-- =========================================================================

	local title = wibox.widget({
		text = w.title or (th.title or "Launcher"),
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local header = wibox.widget({
		{
			title,
			halign = "left",
			valign = "center",
			widget = wibox.container.place,
		},
		left = pick(th.header_pad_l, th.pad_h, 12),
		right = pick(th.header_pad_r, th.pad_h, 12),
		top = pick(th.header_pad_v, th.pad_v, 6),
		bottom = pick(th.header_pad_v, th.pad_v, 6),
		widget = wibox.container.margin,
	})

	header = wibox.widget({
		header,
		bg = pick(th.panel_header_bg, th.header_bg, "#235CDB"),
		fg = pick(th.panel_header_fg, th.header_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- =========================================================================
	-- Body
	-- =========================================================================

	local body = wibox.widget({
		{
			w.body,
			left = pick(th.panel_pad_h, th.pad_h, 12),
			right = pick(th.panel_pad_h, th.pad_h, 12),
			top = pick(th.panel_pad_v, th.pad_v, 12),
			bottom = pick(th.panel_pad_v, th.pad_v, 12),
			widget = wibox.container.margin,
		},
		bg = pick(th.panel_body_bg, th.body_bg, "#1d2f6f"),
		fg = pick(th.panel_body_fg, th.body_fg, "#FFFFFF"),
		widget = wibox.container.background,
	})

	-- =========================================================================
	-- Footer
	-- =========================================================================

	local footer = nil
	do
		local footer_buttons = w.footer_buttons or {}

		if not w.footer and #footer_buttons > 0 then
			local buttons_row = wibox.layout.fixed.horizontal()
			buttons_row.spacing = pick(th.footer_spacing, 8)

			for _, btn in ipairs(footer_buttons) do
				if btn then
					buttons_row:add(btn)
				end
			end

			local row = wibox.widget({
				nil,
				buttons_row,
				expand = "outside",
				layout = wibox.layout.align.horizontal,
			})

			footer = wibox.widget({
				{
					row,
					halign = "right",
					valign = "center",
					widget = wibox.container.place,
				},
				left = pick(th.footer_pad_h, th.pad_h, 12),
				right = pick(th.footer_pad_h, th.pad_h, 12),
				top = pick(th.footer_pad_v, th.pad_v, 8),
				bottom = pick(th.footer_pad_v, th.pad_v, 8),
				widget = wibox.container.margin,
			})

			footer = wibox.widget({
				footer,
				bg = pick(th.panel_footer_bg, th.footer_bg, pick(th.panel_body_bg, "#1d2f6f")),
				fg = pick(th.panel_footer_fg, th.footer_fg, "#FFFFFF"),
				widget = wibox.container.background,
			})
		else
			footer = w.footer
		end
	end

	-- =========================================================================
	-- Dimensions
	-- =========================================================================

	dims.header_h = header_h
	dims.footer_h = footer and (footer_h or 40) or 0
	dims.body_h = math.max(0, dims.h - dims.header_h - dims.footer_h)

	-- =========================================================================
	-- Stack
	-- =========================================================================

	local stack = wibox.widget({
		{
			header,
			strategy = "exact",
			height = dims.header_h,
			widget = wibox.container.constraint,
		},
		{
			body,
			strategy = "exact",
			height = dims.body_h,
			widget = wibox.container.constraint,
		},
		layout = wibox.layout.fixed.vertical,
	})

	if footer then
		stack = wibox.widget({
			stack,
			{
				footer,
				strategy = "exact",
				height = dims.footer_h,
				widget = wibox.container.constraint,
			},
			layout = wibox.layout.fixed.vertical,
		})
	end

	-- =========================================================================
	-- Outer Frame
	-- =========================================================================

	local inner_radius = math.max(0, radius - border_w)

	local inner_bg = wibox.widget({
		{
			stack,
			widget = wibox.container.background,
		},
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, inner_radius)
		end,
		shape_clip = true,
		bg = "#00000000",
		widget = wibox.container.background,
	})

	local inset = wibox.widget({
		inner_bg,
		left = border_w,
		right = border_w,
		top = border_w,
		bottom = border_w,
		widget = wibox.container.margin,
	})

	return wibox.widget({
		inset,
		shape = function(cr, w, h)
			gears.shape.rounded_rect(cr, w, h, radius)
		end,
		shape_clip = true,
		bg = border_color,
		widget = wibox.container.background,
	})
end

return M
